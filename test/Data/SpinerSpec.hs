{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-}

module Data.SpinerSpec where

import Numeric.LinearAlgebra
import Test.Tasty

suite :: TestTree
suite = testGroup "Test Suite"
  [
  ]

-----------------------------------------------------------------------------------------

-- | Approximate equality for doubles.
daeq :: Double  -- ^ epsilon value
     -> Double  -- ^ expected value
     -> Double  -- ^ actual value
     -> Bool    -- ^ true if the expected value is closer than epsilon to actual
daeq d e a = 
  let d' = abs d
  in abs (e - a) < d'

-- | Approximate equality for matrices.
maeq :: Double         -- ^ epsilon value
     -> Matrix Double  -- ^ expected value
     -> Matrix Double  -- ^ actual value
     -> Bool           -- ^ true if all elements are closer to actual than epsilon
maeq d e a =
  if (rows e == rows a) && (cols e == cols a)
    then
      let ePairs = zip (toList $ flatten e) (toList $ flatten a)
          cmp (x, y) = daeq d x y
      in and (cmp <$> ePairs)
    else
      False
