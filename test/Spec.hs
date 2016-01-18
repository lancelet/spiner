import Test.Tasty

import qualified Data.SpinerSpec (suite)

main :: IO ()
main = defaultMain topSuite

topSuite :: TestTree
topSuite = testGroup "Top-Level Suite"
  [ Data.SpinerSpec.suite
  ]
