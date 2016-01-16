module Data.Spiner where

import           Data.Foldable         (foldl')
import qualified Data.Vector.Storable  as VS
import           Numeric.LinearAlgebra

type ActivationFn = Double -> Double
type Weights = Matrix Double

-- | Perform feed-forward propagation of a multi-layered ANN.
--
-- NOTE: The matrix of weights for each layer must be 1 column wider than the number of 
--       inputs, with the first column being a weight for the constant factor.
feedForward :: ActivationFn   -- ^ activation function for all neurons
            -> [ Weights ]    -- ^ matrices of layer weights
            -> Vector Double  -- ^ input vector (excluding constant term)
            -> Vector Double  -- ^ output vector
feedForward g ws i = foldl' (evalLayer g) i ws

-- | Evaluate the outputs of a single layer in a feed-forward ANN.
--
-- This evaluates (apologies for the pseudo-math layout):
--   y = g ( W * [ 1 ] )
--         (     [ x ] )
-- Where y is the output vector, x is the input vector, W are the weights and g is the
-- activation function. g is applied to every element of the resulting vector
-- independently.
--
-- NOTE: The matrix of weights must be 1 column wider than the input vector, with the
--       first column being a weight for the constant factor.
evalLayer :: ActivationFn    -- ^ activation function for all neurons
          -> Vector Double   -- ^ input vector (excluding constant term), length n
          -> Weights         -- ^ matrix of layer weights, size m x (n + 1)
          -> Vector Double   -- ^ output vector, length m
evalLayer g i w = VS.map g (w #> i)
