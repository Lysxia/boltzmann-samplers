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

class AsSpeciesIf (c :: Maybe *) r a d where
  asSpeciesIf
    :: (Alternative f, c ~ LookupM a r, Aliasing r f)
    => Alias r f -> Pay f -> Species f d -> f a

instance AsSpecies a d => AsSpeciesIf 'Nothing r a d where
  asSpeciesIf _ = asSpecies

instance (Aliasing_ a r, Assoc b d b) => AsSpeciesIf ('Just b) r a d where
  asSpeciesIf alias x s = alias $~ species @b s

type family TypesIn (r :: [(*, *)]) a (as :: [*]) :: [*]

type TypesIn' r a = TypesIn r a '[]

type Types r a (as :: [*]) = TypesIfElem (Elem a as) r a as

type family Elem a (as :: [*]) :: Bool where
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as
  Elem a '[] = 'False

type family TypesIfElem (b :: Bool) (r :: [(*, *)]) a (as :: [*]) :: [*] where
  TypesIfElem 'True r a as = as
  TypesIfElem 'False r a as = TypesIfAlias (LookupM a r) r a (a ': as)

type family TypesIfAlias (b :: Maybe *) (r :: [(*, *)]) a (as :: [*]) :: [*] where
  TypesIfAlias 'Nothing r a as = TypesIn r a as
  TypesIfAlias ('Just b) r a as = Types r b as

type GTypesIn r a as = GTypesIn_ r (Rep a) as

type family GTypesIn_ r (f :: * -> *) (as :: [*]) :: [*] where
  GTypesIn_ r (K1 i a) as = Types r a as
  GTypesIn_ r (f :*: g) as = GTypesIn_ r f (GTypesIn_ r g as)
  GTypesIn_ r (f :+: g) as = GTypesIn_ r f (GTypesIn_ r g as)
  GTypesIn_ r (M1 i c f) as = GTypesIn_ r f as
  GTypesIn_ r U1 as = as
  GTypesIn_ r V1 as = as

class AsSpecies' r d d0 where
  asSpecies' :: (Alternative f, Aliasing r f) => Alias r f -> Pay f -> Species f d0 -> Species' f d d0

instance AsSpecies' r '[] d0 where
  asSpecies' _ _ _ = none

instance
  ( AsSpeciesIf (LookupM a r) r a d0, AsSpecies' r d d0
  ) => AsSpecies' r ('(a, a) ': d) d0 where
  asSpecies' alias x s = asSpeciesIf alias x s /\ asSpecies' alias x s

asSystem :: (Alternative f, Aliasing r f, AsSpecies' r d d) => System_ r f d
asSystem = system asSpecies'

type family Duplicate (as :: [*]) :: [(*, *)] where
  Duplicate (a ': as) = ('(a, a) ': Duplicate as)

asSystemFor
  :: forall a r f d
  .  (Alternative f, Aliasing r f, d ~ Duplicate (TypesIn' r a), AsSpecies' r d d)
  => System_ r f d
asSystemFor = asSystem
