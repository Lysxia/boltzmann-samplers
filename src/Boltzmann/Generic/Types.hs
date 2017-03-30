module Boltzmann.Generic.Types where

import Data.Word

class Monad m => MonadRandomLike m where
  -- | Called for every constructor. Counter for ceiled rejection sampling.
  incr :: m ()
  incr = return ()

  word64 :: Word64 -> m Word64
