-- | Solve systems of equations

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Boltzmann.Solver where

import Control.Applicative
import Data.AEq ( (~==) )
import Data.Ord
import Numeric.AD.Mode
import Numeric.AD.Mode.Forward
import Numeric.LinearAlgebra
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S

data SolveArgs = SolveArgs
  { accuracy :: Double
  , numIterations :: Int
  } deriving (Eq, Ord, Show)

defSolveArgs :: SolveArgs
defSolveArgs = SolveArgs 1e-8 20

newton
  :: (Element x, Ord x, Fractional x, Field x, Num (Vector x))
  => (forall s. V.Vector (AD s (Forward x)) -> V.Vector (AD s (Forward x)))
  -> Vector x
  -> [(V.Vector x, V.Vector x)]
newton f x_
  | V.maximumBy (comparing abs) y == 1/0 = []
  | all (== 0) y = [(x, y)]
  | otherwise = (x, y) : newton f x'
  where
    x' = x_ - j_ <\> y_
    j_ = (fromRows . V.toList . fmap V.convert) j
    y_ = V.convert y
    (y, j) = V.unzip (jacobian' f x)
    x = S.convert x_

findZero
  :: SolveArgs
  -> (forall s. V.Vector (AD s (Forward R)) -> V.Vector (AD s (Forward R)))
  -> V.Vector R
  -> Maybe (V.Vector R)
findZero SolveArgs{..} f x0 =
  headM . dropWhile (\(_, y) -> maximum y > accuracy) $ take numIterations iters
  where
    iters = newton f (V.convert x0)
    headM [] = Nothing
    headM ((x, _) : _) = Just x

fixedPoint
  :: SolveArgs
  -> (forall a. (Mode a, Scalar a ~ R) => V.Vector a -> V.Vector a)
  -> V.Vector R
  -> Maybe (V.Vector R)
fixedPoint args f = findZero args (liftA2 (V.zipWith (-)) f id)

-- | Assuming @p . f@ is satisfied only for positive values in some interval
-- @(0, r]@, find @f r@.
search :: (Double -> a) -> (a -> Bool) -> (Double, a)
search f p = search' e0 (0 : [2 ^ n | n <- [0 .. 100 :: Int]])
  where
    search' y (x : xs@(x' : _))
      | p y' = search' y' xs
      | otherwise = search'' y x x'
      where y' = f x'
    search' _ _ = error "Solution not found. Uncontradictable predicate?"
    search'' y x x'
      | x ~== x' = (x, y)
      | p y_ = search'' y_ x_ x'
      | otherwise = search'' y x x_
      where
        x_ = (x + x') / 2
        y_ = f x_
    e0 = error "Solution not found. Unsatisfiable predicate?"
