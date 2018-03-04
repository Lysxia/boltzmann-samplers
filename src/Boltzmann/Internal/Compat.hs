{-# LANGUAGE CPP #-}

module Boltzmann.Internal.Compat
  ( Any
  ) where

#if MIN_VERSION_ghc_prim(0,5,1)
import GHC.Types (Any)
#else
import GHC.Prim (Any)
#endif
