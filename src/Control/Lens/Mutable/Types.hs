{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

module Control.Lens.Mutable.Types where

import           Control.Lens.Lens       (ALens', cloneLens)
import           Control.Lens.Type       (Lens', LensLike')
import           Control.Monad.Primitive (PrimBase (..), PrimMonad (..))
import           GHC.Conc                (STM (..))
import           GHC.Exts                (RealWorld, State#)
import           GHC.Generics            (Generic)

-- | GHC implements different primitive operations, some of which cannot be
-- mixed together and some of which can only be run in certain contexts. In
-- particular, 'STM'-related primops cannot be run directly in the 'IO' monad.
-- However, this restriction is not represented at the bottom layer of the 'IO'
-- runtime which we need to wrap around and expose to users.
--
-- This data structure is our ad-hoc attempt to group together "compatible"
-- primops so that only lens representing compatible references can be composed
-- together, avoiding deadly segfaults.
--
-- See https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/prelude/primops.txt.pp
--
-- See also https://github.com/haskell/primitive/issues/43#issuecomment-613771394
data PrimOpGroup = OpST | OpMVar | OpSTM
  deriving (Read, Show, Generic, Eq, Ord, Enum, Bounded)

-- | Lifted 'State#'. This is needed to interoperate lifted ("normal") types
-- and unlifted types (such as primitives), but it also gives us the chance to
-- restrict composition based on 'PrimOpGroup' which sadly isn't done in the
-- unlifted internal representation (though it could be).
data S (p :: PrimOpGroup) s = S (State# s)

-- | A lifted primitive state-transformer that interoperates with lens.
--
-- Specifically, this is a bare (unwrapped in @StateT@) state transition on a
-- lifted ("normal") state type.
--
-- To obtain one of these, you may apply a @'SLens' p s a@ to a bare state
-- transition, i.e. a function of type @(a -> (r, a))@.
type LST p s r = S p s -> (r, S p s)

-- | Convert an @'LST' p@ to some context @m@.
--
-- This is similar to 'PrimMonad' from the @primitives@ package except our
-- extra @p@ type-param helps us avoid accidentally mixing incompatible primops.
class FromLST p s m where
  stToM :: LST p s r -> m r

-- | Convert an @'LST' p@ to and from some context @m@.
--
-- This is similar to 'PrimBase' from the @primitives@ package except our extra
-- @p@ type-param helps us avoid accidentally mixing incompatible primops.

class FromLST p s m => IsoLST p s m where
  mToST :: m r -> LST p s r

instance (PrimMonad m, s ~ PrimState m) => FromLST 'OpST s m where
  stToM st = primitive $ \s1# -> let !(a, S s2#) = st (S s1#) in (# s2#, a #)

instance (PrimBase m, s ~ PrimState m) => IsoLST 'OpST s m where
  mToST prim (S s1#) = let !(# s2#, a #) = internal prim s1# in (a, S s2#)

-- same as OpST, we just forcibly keep them apart to be safe
instance FromLST 'OpMVar RealWorld IO where
  stToM st = primitive $ \s1# -> let !(a, S s2#) = st (S s1#) in (# s2#, a #)

instance IsoLST 'OpMVar RealWorld IO where
  mToST prim (S s1#) = let !(# s2#, a #) = internal prim s1# in (a, S s2#)

instance FromLST 'OpSTM RealWorld STM where
  stToM st = STM $ \s1# -> let !(a, S s2#) = st (S s1#) in (# s2#, a #)

instance IsoLST 'OpSTM RealWorld STM where
  mToST (STM state#) (S s1#) = let !(# s2#, a #) = state# s1# in (a, S s2#)

-- | Convert an @'LST p@ from some monadic action @m@.
type MonadLST p s m = (FromLST p s m, Monad m)

-- | Representation of a mutable reference as a 'Lens''.
--
-- When the lens functor type-param is @(,) r@, then the output transition
-- function is of type @'LST' s r@. To use it as a monadic action e.g. to run
-- it, you'll need to first convert it using 'stToM'.
--
-- Again, in principle this ought not to be necessary, but the Haskell runtime
-- forces us to do this due to historical design decisions to hide necessary
-- details that seemed appropriate to hide at the time.
type SLens p s a = Lens' (S p s) a

-- | Representation of a mutable reference as a 'ALens''.
--
-- This type is useful if you need to store a lens in a container. To recover
-- the original type, pass it through 'Control.Lens.cloneLens'.
type ASLens p s a = ALens' (S p s) a

-- ** Convenience functions

-- These are all compositions of the basic functions above, provided for
-- convenience rather than necessity.

-- | Run a bare state transition on a lens in the monad for @p@.
--
-- The lens may be an @'SLens' p@ or any compositions of it with other optics,
-- including prisms and so forth.
runSLens :: FromLST p s m => LensLike' ((,) r) (S p s) a -> (a -> (r, a)) -> m r
runSLens = fmap stToM

-- | Run a bare state transition on an 'ALens'' in the monad for @p@.
runASLens :: FromLST p s m => ALens' (S p s) a -> (a -> (r, a)) -> m r
runASLens = runSLens . cloneLens

-- | A bare state transition representing a read operation.
stateRead :: a -> (a, a)
stateRead a = (a, a)

-- | A bare state transition representing a write operation.
--
-- @'stateWrite' b@ can be passed to 'runSLens' to write @b@ to the reference.
stateWrite :: b -> a -> ((), b)
stateWrite b a = ((), b)

-- | A bare state transition representing a modify/map operation.
--
-- @'stateModify' f@ can be passed to 'runSLens' to apply @f@ to the reference.
stateModify :: (a -> b) -> a -> ((), b)
stateModify f a = ((), f a)
