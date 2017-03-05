{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad
import Data.Data
import Data.Foldable
import Data.IORef
import Data.List
import System.Exit
import System.IO

import Options.Generic

import Boltzmann.Data
import Boltzmann.Data.Data

import Test.Stats

data T = L | N T T
  deriving (Eq, Ord, Show, Data)

size :: T -> Int
size (N l r) = 1 + size l + size r
size L = 0

eps, del :: Double
eps = 0.01
del = 0.001

-- | Periodically print stuff so that Travis does not think we're stuck.
counting x gen = do
  modifyIORef x (+ 1)
  readIORef x >>= \x ->
    when (x `mod` 1000 == 0) $ putStr "." >> hFlush stdout
  gen

-- | Invocation: stack test [--test-arguments TEST_SIZE]
type Input = Maybe (Int <?> "Test size")

main = do
  n_ <- getRecord "Test program" :: IO Input
  success <- newIORef True

  let n = maybe 10 unHelpful n_
      range = tolerance epsilon n

  for_
    [ ( "reject "
      , generatorSR
      , expected Nothing range eps del
      )
    , ( "rejectSimple "
      , generatorR'
      , expected (Just (fromIntegral n)) range eps del
      )
    ] $ \(name, g, kdist) -> do
    putStrLn $ name ++ show n
    let gen = (fmap size . asMonadRandom . g) n
    x <- newIORef 0
    (expectedDist, estimatedDist, diff) <- runExperiment kdist (counting x gen)
    putStrLn ""
    when (diff > eps) $ do
      writeIORef success False
      putStrLn $ "FAIL > " ++ show diff
      print expectedDist
      print estimatedDist

{-
  let k = 80000
      eps = 0.1
      gen = (fmap size . asMonadRandom . generatorP') n
  putStrLn $ "pointed " ++ show n
  x <- newIORef 0
  sizes <- replicateM k (counting x gen)
  putStrLn ""
  let diff = abs (mean sizes - fromIntegral (n `div` 2))
  when (diff > eps) $ do
    writeIORef success False
    putStrLn $ "FAIL > " ++ show diff
-}

  success <- readIORef success
  unless success exitFailure
