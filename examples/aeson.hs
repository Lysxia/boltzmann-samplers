{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Aeson
import qualified Data.HashMap.Lazy as HashMap
import Data.Scientific
import qualified Data.Vector as Vector
import Data.Text ( Text )
import qualified Data.Text as Text
import Test.QuickCheck
import GHC.Generics
import Boltzmann.Options
import Boltzmann.Species.System
import Boltzmann.Species.Generic

import Data.Typeable
import System.Exit

instance
  ( Assoc Object d Object
  , Assoc Array d Array
  , Assoc Text d Text
  , Assoc Scientific d Scientific
  , Assoc Bool d Bool
  , Assoc Value d Value
  ) => AsSpecies Value d

type instance TypesIn Value = GTypesIn Value

instance WAlternative Double Gen where
  wempty = fail "wempty Gen"
  wplus (x, m) (y, n) = do
    p <- choose (0, x+y)
    if p > x then m else n

instance Arbitrary Value where
  arbitrary = sizedGeneratorFor aliases (Options (Just (fromIntegral 2)) 1)

type R_ = '[ '(Object, [(Text, Value)]), '(Array, [Value]),
                            '(Scientific, ()), '(Text, String), '(String, [()])]

aliases :: FAlias R_ Gen
aliases =
     fmap (HashMap.fromList :: [(Text, Value)] -> Object)
  :& fmap (Vector.fromList :: [Value] -> Array)
  :& (const (fmap (realToFrac :: Double -> Scientific) arbitrary) :: Gen () -> Gen Scientific)
  :& fmap Text.pack
  :& (const arbitrary :: Gen [()] -> Gen String)
  :& ANil

main :: IO ()
main = do
  generate (arbitrary :: Gen Value) >>= \x -> x `seq` exitFailure
  sample (arbitrary :: Gen Value)
  quickCheck $ \(v :: Value) -> (decode . encode) v === Just v
