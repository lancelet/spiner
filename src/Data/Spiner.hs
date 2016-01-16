module Data.Spiner where

import           Data.Foldable         (foldl')
import qualified Data.Vector.Storable  as VS
import           Numeric.LinearAlgebra

type ActivationFn = Double -> Double
type Weights = Matrix Double

feedForward :: ActivationFn -> [ Weights ] -> Vector Double -> Vector Double
feedForward g ws i = foldl' (evalLayer g) i ws

evalLayer :: ActivationFn -> Vector Double -> Weights -> Vector Double
evalLayer g i w = VS.map g (w #> i)
