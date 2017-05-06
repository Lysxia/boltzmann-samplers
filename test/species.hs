{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Random
import Boltzmann.Species
import Boltzmann.Species.System
import Test.Tree

tgtSize = 10
iters = 10000
-- cumulatedSize approx. 100000

generator :: IO T
generator = sizedGenerator @_ @"tree" ANil s (sizedOptions tgtSize)

instance WAlternative Double IO where
  wempty = fail "wempty"
  wplus (x, a) (y, b) = do
    p <- getRandomR (0, x + y)
    if p < x then a else b

main :: IO ()
main = go 0 iters
  where
    go !x 0 = print x
    go x n = do
      t <- generator
      let x' = size t + x
      go x' (n - 1)
