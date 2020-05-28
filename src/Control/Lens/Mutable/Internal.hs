{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UnboxedTuples         #-}

module Control.Lens.Mutable.Internal where

-- external
import           Control.Concurrent.STM.TMVar (TMVar)
import           Control.Lens                 (Lens')
import           Data.Primitive.MutVar        (MutVar (..))
import           GHC.Conc                     (TVar (..))
import           GHC.Exts                     (MVar#, RealWorld, State#,
                                               newMVar#, newMutVar#, newTVar#,
                                               putMVar#, readMutVar#, readTVar#,
                                               retry#, takeMVar#, writeMutVar#,
                                               writeTVar#)
import           GHC.IORef                    (IORef (..))
import           GHC.MVar                     (MVar (..))
import           GHC.STRef                    (STRef (..))
import           Unsafe.Coerce                (unsafeCoerce)

-- internal
import           Control.Lens.Mutable.Types


-- | Convert a reference type to a 'Lens''.
class AsLens s a ref where
  asLens :: ref a -> Lens' s a

instance AsLens (S 'OpST s) a (MutVar s) where
  asLens (MutVar var#) f (S s1#) =
    let !(# s2#, valr #) = readMutVar# var# s1#
    in  fmap (\valw -> S (writeMutVar# var# valw s2#)) (f valr)

instance AsLens (S 'OpST s) a (STRef s) where
  asLens (STRef var#) f (S s1#) =
    let !(# s2#, valr #) = readMutVar# var# s1#
    in  fmap (\valw -> S (writeMutVar# var# valw s2#)) (f valr)

instance AsLens (S 'OpST RealWorld) a IORef where
  asLens (IORef stref) = asLens stref

-- | View a @'MVar' a@ as a @'SLens' \''OpST' 'RealWorld' a@.
--
-- Note: when this is eventually run in 'IO', the action will block the thread
-- until there is a value present, as per the semantics of 'takeMVar#'. It will
-- then put a value into the 'MVar', which will block the thread if the value
-- is absent. GHC doesn't give atomicity guarantees for 'MVar' so it's possible
-- this does happen, e.g. if another producer managed to "get in there" during
-- the intervening period between the two operations. Unfortunately GHC does
-- not provide an atomic @modifyMVar@ function or primop.
--
-- If you don't want to deal with this, don't use an 'MVar', use a 'TMVar'.
instance AsLens (S 'OpMVar RealWorld) a MVar where
  asLens (MVar var#) f (S s1#) =
    let !(# s2#, valr #) = takeMVar# var# s1#
    in  fmap (\valw -> S (putMVar# var# valw s2#)) (f valr)

instance AsLens (S 'OpSTM RealWorld) a TVar where
  asLens (TVar var#) f (S s1#) =
    let !(# s2#, valr #) = readTVar# var# s1#
    in  fmap (\valw -> S (writeTVar# var# valw s2#)) (f valr)

instance AsLens (S 'OpSTM RealWorld) a TMVar where
  asLens (tmvar :: TMVar a) f (S s1#) =
    -- they hide the constructor (no exports) but it's just a newtype
    let !(TVar var#)      = (unsafeCoerce tmvar :: TVar (Maybe a))
        !(# s2#, valr' #) = readTVar# var# s1#
        valr              = case valr' of
          Just v  -> v
          Nothing -> let (# _, a #) = retry# s1# in a
    in  fmap (\valw -> S (writeTVar# var# (Just valw) s2#)) (f valr)


-- | A state in which you can allocate new references.
--
-- This can be defined on either pure or impure references. For pure references
-- one could e.g. define an instance of this on @Map k v@ with @Const k@ as the
-- reference type - see unit tests for an example.
class AsLens s a ref => Allocable s a ref where

  -- | Allocate a new reference with the given value.
  alloc :: a -> s -> (ref a, s)

  {- | Deallocate an existing reference, and return its value.

  The default implementation simply writes 'error' into the reference and
  returns the old value. The caller is responsible for actually throwing away
  the reference and never using it again, as per Haskell's GC semantics.
  -}
  free :: ref a -> s -> (a, s)
  free ref = asLens ref (, error "use-after-free")

  {- | Check if a reference is valid.

  The default implementation simply forces the reference and returns 'True'. If
  the reference has already been freed (via 'free') then an error will be
  raised, which you can catch in the 'IO' monad as per usual. In other words,
  the default implementation will never return 'False'.
  -}
  isValid :: ref a -> s -> (Bool, s)
  isValid ref = asLens ref $ \r -> (r `seq` True, r)

instance Allocable (S 'OpST s) a (MutVar s) where
  alloc val (S s1#) =
    let !(# s2#, var# #) = newMutVar# val s1# in (MutVar var#, S s2#)

instance Allocable (S 'OpST s) a (STRef s) where
  alloc val (S s1#) =
    let !(# s2#, var# #) = newMutVar# val s1# in (STRef var#, S s2#)

instance Allocable (S 'OpST RealWorld) a IORef where
  alloc val s = let (r, s') = alloc val s in (IORef r, s')

instance Allocable (S 'OpMVar RealWorld) a MVar where
  alloc (val :: a) (S s1#) =
    let !(# s2#, var# #) =
            newMVar# s1# :: (# State# RealWorld, MVar# RealWorld a #)
    in  (MVar var#, S (putMVar# var# val s2#))

instance Allocable (S 'OpSTM RealWorld) a TVar where
  alloc val (S s1#) =
    let !(# s2#, var# #) = newTVar# val s1# in (TVar var#, S s2#)

instance Allocable (S 'OpSTM RealWorld) a TMVar where
  alloc val (S s1#) =
    let !(# s2#, var# #) = newTVar# (Just val) s1#
    in  (unsafeCoerce (TVar var#), S s2#)
