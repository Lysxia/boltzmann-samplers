{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Boltzmann.Species.Generic where

import Control.Applicative
import Boltzmann.Species.System
import Generics.OneLiner
import GHC.Generics
import GHC.TypeLits

import Data.TypeMap.Static

class AsSpecies a d where
  asSpecies :: Alternative f => Pay f -> Species f d -> f a

  default asSpecies
    :: (Alternative f, GAsSpecies a d)
    => Pay f -> Species f d -> f a
  asSpecies = gspecies

class (KnownNat (Index a d), Lookup a d ~ a) => Assoc' d a
instance (KnownNat (Index a d), Lookup a d ~ a) => Assoc' d a

type GAsSpecies a d = (ADT a, Constraints a (Assoc' d))

gspecies
  :: forall f d a
  .  (Alternative f, GAsSpecies a d)
  => Pay f -> Species f d -> f a
gspecies x s = x $
  createA (For :: For (Assoc' d)) (species @a s :: forall a. Assoc' d a => f a)

type family TypesIn a (as :: [*]) :: [*]

type TypesIn' a = TypesIn a '[]

type Types a (as :: [*]) = TypesIfElem (Elem a as) a as

type family Elem a (as :: [*]) :: Bool where
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as
  Elem a '[] = 'False

type family TypesIfElem (b :: Bool) a (as :: [*]) :: [*] where
  TypesIfElem 'True a as = as
  TypesIfElem 'False a as = TypesIn a (a ': as)

type GTypesIn a as = GTypesIn_ (Rep a) as

type family GTypesIn_ (f :: * -> *) (as :: [*]) :: [*] where
  GTypesIn_ (K1 i a) as = Types a as
  GTypesIn_ (f :*: g) as = GTypesIn_ f (GTypesIn_ g as)
  GTypesIn_ (f :+: g) as = GTypesIn_ f (GTypesIn_ g as)
  GTypesIn_ (M1 i c f) as = GTypesIn_ f as
  GTypesIn_ U1 as = as
  GTypesIn_ V1 as = as

class AsSpecies' d d0 where
  asSpecies' :: Alternative f => Pay f -> Species f d0 -> Species' f d d0

instance AsSpecies' '[] d0 where
  asSpecies' _ _ = none

instance (AsSpecies a d0, AsSpecies' d d0) => AsSpecies' ('(a, a) ': d) d0 where
  asSpecies' x s = asSpecies x s /\ asSpecies' x s

asSystem :: (Alternative f, AsSpecies' d d) => System_ f d
asSystem = system asSpecies'

type family Duplicate (as :: [*]) :: [(*, *)] where
  Duplicate (a ': as) = ('(a, a) ': Duplicate as)

asSystemFor
  :: forall a f d
  .  (Alternative f, d ~ Duplicate (TypesIn' a), AsSpecies' d d)
  => System_ f d
asSystemFor = asSystem
