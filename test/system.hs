{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Vector as V

import Boltzmann.Species
import Boltzmann.Species.System
import Test.Tree

main = defaultMain
  [ testCase "num" $
      [1, 98, 8] @=?
        V.toList (applySystemGF s 2 (V.fromList [3, 5, 7]))
  , testCase "pointed_num" $
      V.fromList
        [  1,   0
        , 98, 378 {- 0 + 98 + 2 * (7 * 10 + 10 * 7) -}
        ,  8,  12 {- 4 + 8 -}
        ] @=?
        unshape 2 (applySystemPGF s 2 (V.fromList [[3, 4], [5, 8], [7, 10]]))
  , testCase "coeffs" $
      [1, 1, 2] @=?
        let Wrapped (Coefficients cs) = lookupSys @"tree" s
        in take 3 cs
  , testCase "pointed" $
      [[1, 1, 2], [0, 1, 4], [0, 1, 6]] @=?
        let Wrapped (Pointed css) = lookupSys @"tree" s
        in [[c | c <- take 3 cs] | Coefficients cs <- take 3 css]
  , testCase "shape" $
      let v = V.fromList [0 .. 20]
      in v @=? (unshape 7 . shape 3 7) v
  , testCase "unshape" $
      V.fromList (replicate 6 0) @=?
        unshape 2 (V.fromList (replicate 3 (repeat (0 :: Int))))
  , testCase "solveAt" $
      case solveAt @D s 0 0.15 of
        Nothing -> assertFailure "Solver failed"
        Just x -> do
          let y = (unshape 2 . applySystemPGF s 0.15 . shape 3 2) x
          if all (\(x, y) -> abs (x - y) < 10 ** (-4)) (V.zip x y) then
            return ()
          else
            print (x, y) >> assertFailure "Not a fixpoint"
  , testCase "solveSized" $ do
      let (x, v) = solveSized @"tree" @D s (sizedOptions 10)
          [z, z'] = v V.! 2
      if abs (10 - z' / z) < 10 ** (-4) then
        return ()
      else
        print (x, v) >> assertFailure "Wrong size"
  ]
