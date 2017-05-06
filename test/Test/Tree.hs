{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tree where

import Boltzmann.Species

data T = L | N T T
  deriving Show

size :: T -> Int
size L = 0
size (N l r) = 1 + size l + size r

type D = '[ '("leaf", T), '("node", T), '("tree", T)]

s :: System '[] D
s = system $ \_ pay s ->
  ( pure L
  ) /\
  ( let tree = species @"tree" s
    in pay (N <$> tree <*> tree)
  ) /\
  ( let leaf = species @"leaf" s
        node = species @"node" s
    in leaf <|> node
  ) /\
  none

