{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Lens.Mutable
import           Control.Monad.Extra   (whileJustM)
import           Data.Foldable         (for_)
import           Data.Primitive.MutVar
import           GHC.Exts
import           System.CPUTime
import           System.Mem
import           Text.Printf

iterations :: Int
iterations = 400000000

testRef :: IO ()
testRef = do
  r <- newMutVar 0
  whileJustM $ do
    v <- readMutVar r
    --threadDelay 1
    writeMutVar r (v + 1)
    if v < iterations then pure $ Just () else pure Nothing

testMutLens :: IO ()
testMutLens = do
  r <- stToM $ alloc @(S 'OpST RealWorld) @Int @(MutVar RealWorld) 0
  whileJustM $ do
    v <- stToM $ asLens @(S 'OpST RealWorld) @Int @(MutVar RealWorld) r $ \x ->
      (x, x)
    --threadDelay 1
    stToM $ asLens @(S 'OpST RealWorld) @Int @(MutVar RealWorld) r $ \x ->
      ((), x + 1)
    if v < iterations then pure $ Just () else pure Nothing

time :: String -> IO a -> IO a
time n a = do
  start <- getCPUTime
  putStrLn $ n <> ": start"
  r   <- a
  end <- getCPUTime
  printf "%s: elapsed %6d ms\n" n ((end - start) `div` 1000000000)
  pure r

main :: IO ()
main = do
  for_ [0 .. 2 :: Int] $ \_ -> do
    time "testRef    " testRef
    performGC
    time "testMutLens" testMutLens
    performGC
