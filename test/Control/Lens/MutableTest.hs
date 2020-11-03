{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}

module Control.Lens.MutableTest where

-- external, testing
import           Test.Tasty
import           Test.Tasty.HUnit

-- external
import qualified Data.Map                         as M

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.ST
import           Control.Monad.Trans.State.Strict
import           Data.IORef
import           Data.List                        (isInfixOf)
import           Data.Map                         (Map)
import           Data.Maybe                       (fromJust)
import           Data.Primitive.MutVar
import           Data.STRef
import           GHC.Generics                     (Generic)

-- internal
import           Control.Lens.Mutable

type ComplexStruct = ([Int], (Maybe Bool, Either String [(String, Maybe Int)]))

smoke
  :: forall s m ref
   . Monad m
  => Allocable s ComplexStruct ref
  => (forall a . (s -> (a, s)) -> m a)
  -> (ref ComplexStruct -> m ComplexStruct)
  -> (forall a . m a -> IO a)
  -> ComplexStruct
  -> String
  -> IO ()
smoke runPureST readRef runInIO oldVal suf = do
  -- note readRef is not redundant, it is used to check that the thing being
  -- tested (Allocable) matches the expected behaviour (readMutVar etc)
  (oldPart, newVal) <- runInIO $ do
    ref <- runPureST $ alloc oldVal
    let l = asLens @s ref . _2 . _2 . _Right . ix 1 . _1
    oldPart <- runPureST $ l (\v -> (v, v <> " " <> suf))
    newVal  <- readRef ref
    pure (oldPart, newVal)
  assertBool "oldPart is not in oldVal" (oldPart `isInfixOf` show oldVal)
  assertBool "oldPart is not in newVal" (oldPart `isInfixOf` show newVal)
  assertBool "suf is not in newVal"     (suf `isInfixOf` show newVal)

smoke'
  :: forall p s m ref
   . MonadLST p s m
  => Allocable (S p s) ComplexStruct ref
  => (ref ComplexStruct -> m ComplexStruct)
  -> (forall a . m a -> IO a)
  -> ComplexStruct
  -> String
  -> IO ()
smoke' = smoke @(S p s) @m @ref stToM

data AllocMap k v = AllocMap
  { aNext  :: !k
  , aStore :: !(Map k v)
  }
  deriving (Show, Read, Generic, Eq, Ord)
makeLensesFor ((\x -> (x, "_" <> x)) <$> ["aNext", "aStore"]) ''AllocMap

instance Ord k => AsLens (AllocMap k v) v (Const k) where
  asLens (Const k) = _aStore . unsafeSingular (ix k)

instance (Ord k, Enum k) => Allocable (AllocMap k v) v (Const k) where
  alloc v (AllocMap i m) = (Const i, AllocMap (succ i) (M.insert i v m))
  isValid (Const k) (AllocMap i m) = (M.member k m, AllocMap i m)
  free (Const k) am = am & _aStore . at k %%~ \case
    Nothing -> (error "double-free", Nothing)
    Just v  -> (v, Nothing)

type ACS = AllocMap Int ComplexStruct

tests :: TestTree
tests = testGroup
  "Control.Lens.MutableTest"
  [ testCase "smoke tests" $ do
      let oldVal = ([0], (Just True, Right [("red", Nothing), ("blue", Just 4)]))
      let suf    = "excellent"
      smoke' @'OpST readMutVar stToIO oldVal suf
      smoke' @'OpST readSTRef stToIO oldVal suf
      smoke' @'OpST readIORef id oldVal suf
      smoke' @'OpMVar @RealWorld readMVar id oldVal suf
      smoke' @'OpSTM @RealWorld readTVar atomically oldVal suf
      smoke' @'OpSTM @RealWorld readTMVar atomically oldVal suf
      smoke @ACS @(StateT ACS IO) @(Const Int)
        state
        (\(Const k) -> do
          get >>= \(AllocMap i m) -> pure (fromJust $ M.lookup k m)
        )
        (`evalStateT` AllocMap (toEnum 0) mempty)
        oldVal
        suf
  ]
