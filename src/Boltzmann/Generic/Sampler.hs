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

import Control.Applicative
import Data.Word
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

newtype instance OracleG (M1 i c f) d = OGM1 (OracleG f d)
data instance OracleG (f :*: g) d = OGProd (OracleG f d) (OracleG g d)
newtype instance OracleG (f :+: g) d = OGSum (OracleGSum (f :+: g) d)

class GenerableG f (d :: [(*, *)]) where
  generateG :: MonadRandomLike m => OracleG f d -> m (f p)

instance GenerableG U1 d where
  generateG _ = return U1

instance GenerableG f d => GenerableG (M1 i c f) d where
  generateG (OGM1 f) = M1 <$> generateG f

instance (GenerableG f d, GenerableG g d)
  => GenerableG (f :*: g) d
  where
  generateG (OGProd f g) = liftA2 (:*:) (generateG f) (generateG g)

instance GenerableGSum (f :+: g) d
  => GenerableG (f :+: g) d
  where
  generateG (OGSum f) = word64 >>= generateGSum f

data family OracleGSum (f :: * -> *) (d :: [(*, *)])

data instance OracleGSum (f :+: g) d = OGSSum Word64 (OracleGSum f d) (OracleGSum g d)

class GenerableGSum f d where
  generateGSum :: MonadRandomLike m => OracleGSum f d -> Word64 -> m (f p)

instance (GenerableGSum f d, GenerableGSum g d)
  => GenerableGSum (f :+: g) d
  where
  generateGSum (OGSSum v f g) w
    | w < v = L1 <$> generateGSum f w
    | otherwise = R1 <$> generateGSum g w
