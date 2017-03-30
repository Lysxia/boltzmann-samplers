{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Boltzmann.Generic.Sampler where

import GHC.Generics

import Boltzmann.Generic.Types

data Dictionary (d :: [(*, *)]) where
  NilD :: Dictionary '[]
  ConsD :: (a -> b) -> Dictionary d -> Dictionary ('(a, b) ': d)

class GenerableSpecies a (d :: [(*, *)]) where
  type Oracle a (d :: [(*, *)])
  generate :: MonadRandomLike m => proxy d -> Oracle a d -> m a

  type Oracle a d = OracleG (Rep a) d
  default generate
    :: ( Generic a, GenerableG (Rep a) d, Oracle a d ~ OracleG (Rep a) d
       , MonadRandomLike m)
    => proxy d -> Oracle a d -> m a
  generate _ = fmap to . generateG

class OracleSpecies a (d :: [(*, *)]) where
  oracle :: proxy a -> Int -> Dictionary d -> Oracle a d

data family OracleG (f :: * -> *) (d :: [(*, *)])

class GenerableG f (d :: [(*, *)]) where
  generateG :: MonadRandomLike m => OracleG f d -> m (f p)

instance GenerableG U1 d where
  generateG _ = return U1
