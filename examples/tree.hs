{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe
import qualified Data.Vector as V
import Test.QuickCheck
import Boltzmann.Species

data T = L | N T T
  deriving Show

size :: T -> Int
size L = 1
size (N l r) = 1 + size l + size r

s :: System '[ '("leaf", T), '("node", T), '("tree", T)]
s = system $
  equation (pure L) /\
  equation (
    let tree = lookupSys @"tree" s
    in pay (N <$> tree <*> tree)) /\
  equation (
    let leaf = lookupSys @"leaf" s
        node = lookupSys @"node" s
    in leaf <|> node) /\
  emptySys

main = undefined
