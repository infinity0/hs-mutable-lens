import           Test.Tasty

import           Control.Lens.MutableTest (tests)

main :: IO ()
main = do
  defaultMain $ testGroup "Mutable Lens *" [Control.Lens.MutableTest.tests]
