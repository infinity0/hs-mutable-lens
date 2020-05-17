{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Control.Lens.MutableTest where

-- external, testing
import           Test.Tasty
import           Test.Tasty.HUnit

-- external
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Lens            hiding (lens)
import           Control.Monad.ST
import           Data.IORef
import           Data.List               (isInfixOf)
import           Data.Primitive.MutVar
import           Data.STRef

-- internal
import           Control.Lens.Mutable

type ComplexStruct = ([Int], (Maybe Bool, Either String [(String, Maybe Int)]))

smoke
  :: forall p s m ref
   . MonadLST p s m
  => AsLens p s ref
  => (ComplexStruct -> m (ref ComplexStruct))
  -> (ref ComplexStruct -> m ComplexStruct)
  -> (forall a . m a -> IO a)
  -> ComplexStruct
  -> String
  -> IO ()
smoke newRef readRef runInIO oldVal suf = do
  (oldPart, newVal) <- runInIO $ do
    ref <- newRef oldVal
    let lens = asLens @p @s ref . _2 . _2 . _Right . ix 1 . _1
    oldPart <- runSLens lens (\v -> (v, v <> " " <> suf))
    newVal  <- readRef ref
    pure (oldPart, newVal)
  assertBool "oldPart is not in oldVal" (oldPart `isInfixOf` show oldVal)
  assertBool "oldPart is not in newVal" (oldPart `isInfixOf` show newVal)
  assertBool "suf is not in newVal"     (suf `isInfixOf` show newVal)

tests :: TestTree
tests = testGroup
  "Control.Lens.MutableTest"
  [ testCase "smoke tests" $ do
      let oldVal = ([0], (Just True, Right [("red", Nothing), ("blue", Just 4)]))
      let suf    = "excellent"
      smoke @'OpST newMutVar readMutVar stToIO oldVal suf
      smoke @'OpST newSTRef readSTRef stToIO oldVal suf
      smoke @'OpST newIORef readIORef id oldVal suf
      smoke @'OpMVar @RealWorld newMVar readMVar id oldVal suf
      smoke @'OpSTM @RealWorld newTVar readTVar atomically oldVal suf
      smoke @'OpSTM @RealWorld newTMVar readTMVar atomically oldVal suf
  ]
