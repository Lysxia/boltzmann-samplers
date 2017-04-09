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
import Boltzmann.Species.System (Coefficients(..), Pointed(..), Wrapped(..), applySystemGF)
import Test.Tree

main = defaultMain
  [ testCase "num" $
      [1, 98, 8] @=?
        V.toList (applySystemGF s 2 (V.fromList [3, 5, 7]))
  , testCase "coeffs" $
      [1, 1, 2] @=?
        let Wrapped (Coefficients cs) = lookupSys @"tree" s
        in take 3 cs
  , testCase "pointed" $
      [[1, 1, 2], [0, 1, 4], [0, 1, 6]] @=?
        let Wrapped (Pointed css) = lookupSys @"tree" s
        in [[c | c <- take 3 cs] | Coefficients cs <- take 3 css]
  ]
