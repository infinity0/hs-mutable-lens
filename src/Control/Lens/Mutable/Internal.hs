{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnboxedTuples         #-}

module Control.Lens.Mutable.Internal where

-- external
import           Control.Concurrent.STM.TMVar (TMVar)
import           Data.Primitive.MutVar        (MutVar (..))
import           GHC.Conc                     (TVar (..))
import           GHC.Exts                     (RealWorld, putMVar#, readMutVar#,
                                               readTVar#, retry#, takeMVar#,
                                               writeMutVar#, writeTVar#)
import           GHC.IORef                    (IORef (..))
import           GHC.MVar                     (MVar (..))
import           GHC.STRef                    (STRef (..))
import           Unsafe.Coerce                (unsafeCoerce)

-- internal
import           Control.Lens.Mutable.Types


class AsLens p s ref where
  asLens :: ref a -> SLens p s a

instance AsLens 'OpST s (MutVar s) where
  asLens (MutVar var#) f (S s1#) =
    let !(# s2#, valr #) = readMutVar# var# s1#
    in  fmap (\valw -> S (writeMutVar# var# valw s2#)) (f valr)

instance AsLens 'OpST s (STRef s) where
  asLens (STRef var#) f (S s1#) =
    let !(# s2#, valr #) = readMutVar# var# s1#
    in  fmap (\valw -> S (writeMutVar# var# valw s2#)) (f valr)

instance AsLens 'OpST RealWorld IORef where
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
instance AsLens 'OpMVar RealWorld MVar where
  asLens (MVar var#) f (S s1#) =
    let !(# s2#, valr #) = takeMVar# var# s1#
    in  fmap (\valw -> S (putMVar# var# valw s2#)) (f valr)

instance AsLens 'OpSTM RealWorld TVar where
  asLens (TVar var#) f (S s1#) =
    let !(# s2#, valr #) = readTVar# var# s1#
    in  fmap (\valw -> S (writeTVar# var# valw s2#)) (f valr)

instance AsLens 'OpSTM RealWorld TMVar where
  asLens (tmvar :: TMVar a) f (S s1#) =
    -- they hide the constructor (no exports) but it's just a newtype
    let !(TVar var#)      = (unsafeCoerce tmvar :: TVar (Maybe a))
        !(# s2#, valr' #) = readTVar# var# s1#
        valr              = case valr' of
          Just v  -> v
          Nothing -> let (# _, a #) = retry# s1# in a
    in  fmap (\valw -> S (writeTVar# var# (Just valw) s2#)) (f valr)
