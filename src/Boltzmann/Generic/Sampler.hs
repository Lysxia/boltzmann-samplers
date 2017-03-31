{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Boltzmann.Generic.Sampler where

import Control.Applicative
import Data.Proxy
import Data.Word
import GHC.Generics

import Boltzmann.Generic.Types

data Dictionary (d :: [(*, *)]) where
  NilD :: Dictionary '[]
  ConsD :: (a -> b) -> Dictionary d -> Dictionary ('(a, b) ': d)

newtype Oracle a d = Oracle (Oracle' a d d)

class Generable' a d d => Generable a d
instance Generable' a d d => Generable a d

generate :: (Generable a d, MonadRandomLike m) => Oracle a d -> m a
generate o@(Oracle r) = generate' o o r

type family Oracle' a (d :: [(*, *)]) (e :: [(*, *)]) where
  Oracle' a d ('(b, a) ': _) = (b -> a, Oracle b d)
  Oracle' a d (_ ': e) = Oracle' a d e
  Oracle' a d '[] = Oracle_ a d

class Generable' a (d :: [(*, *)]) (e :: [(*, *)]) where
  generate' :: MonadRandomLike m => proxy d -> proxy' e -> Oracle' a d e -> m a

instance {-# OVERLAPPABLE #-}
  ( Oracle' a d (z ': e) ~ Oracle' a d e
  , Generable' a d e
  ) => Generable' a d (z ': e)
  where
  generate' pd pe = generate' pd (proxyTail pe)

proxyTail :: proxy (z ': e) -> Proxy (e :: [(*,*)])
proxyTail _ = Proxy

instance {-# OVERLAPPING #-} Generable b d => Generable' a d ('(b, a) ': e) where
  generate' pd _ (f, o) = f <$> generate o

instance Generable_ a d => Generable' a d '[] where
  generate' pd _ = generate_ pd

class Generable_ a (d :: [(*, *)]) where
  type Oracle_ a (d :: [(*, *)])
  generate_ :: MonadRandomLike m => proxy d -> Oracle_ a d -> m a

  type Oracle_ a d = OracleG (Rep a) d
  default generate_
    :: ( Generic a, GenerableG (Rep a) d, Oracle_ a d ~ OracleG (Rep a) d
       , MonadRandomLike m)
    => proxy d -> Oracle_ a d -> m a
  generate_ _ = fmap to . generateG

data family OracleG (f :: * -> *) (d :: [(*, *)])

newtype instance OracleG (M1 i c f) d = OGM1 (OracleG f d)
data instance OracleG (f :*: g) d = OGProd (OracleG f d) (OracleG g d)
newtype instance OracleG (f :+: g) d = OGSum (OracleGSum (f :+: g) d)
newtype instance OracleG (K1 i c) d = OGK1 (Oracle c d)

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

instance Generable c d => GenerableG (K1 i c) d where
  generateG (OGK1 r) = K1 <$> generate r

data family OracleGSum (f :: * -> *) (d :: [(*, *)])

data instance OracleGSum (f :+: g) d = OGSSum Word64 (OracleGSum f d) (OracleGSum g d)
newtype instance OracleGSum (M1 i c f) d = OGSM1 (OracleG (M1 i c f) d)

class GenerableGSum f d where
  generateGSum :: MonadRandomLike m => OracleGSum f d -> Word64 -> m (f p)

instance (GenerableGSum f d, GenerableGSum g d)
  => GenerableGSum (f :+: g) d
  where
  generateGSum (OGSSum v f g) w
    | w < v = L1 <$> generateGSum f w
    | otherwise = R1 <$> generateGSum g w

instance GenerableG f d => GenerableGSum (M1 i c f) d where
  generateGSum (OGSM1 f) _ = generateG f
