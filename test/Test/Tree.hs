{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tree where

import Boltzmann.Species

data T = L | N T T
  deriving Show

size :: T -> Int
size L = 1
size (N l r) = 1 + size l + size r

type D = '[ '("leaf", T), '("node", T), '("tree", T)]

s :: System D
s = system $
  ( pure L
  ) /\
  ( let tree = lookupSys @"tree" s
    in pay (N <$> tree <*> tree)
  ) /\
  ( let leaf = lookupSys @"leaf" s
        node = lookupSys @"node" s
    in leaf <|> node
  ) /\
  emptySys

