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
import Generics.OneLiner
import GHC.Generics
import GHC.TypeLits

import Data.TypeMap.Static

import Boltzmann.Options
import Boltzmann.Species.System

class AsSpecies a d where
  asSpecies :: Alternative f => Pay f -> Species f d -> f a

  default asSpecies
    :: (Alternative f, GAsSpecies a d)
    => Pay f -> Species f d -> f a
  asSpecies x = x . gspecies

class (KnownNat (Index a d), Lookup a d ~ a) => Assoc' d a
instance (KnownNat (Index a d), Lookup a d ~ a) => Assoc' d a

type GAsSpecies a d = (ADT a, Constraints a (Assoc' d))

gspecies
  :: forall f d a
  .  (Alternative f, GAsSpecies a d)
  => Species f d -> f a
gspecies s =
  createA (For :: For (Assoc' d)) (species @a s :: forall a. Assoc' d a => f a)

class AsSpeciesIf (c :: Maybe *) r a d where
  asSpeciesIf
    :: (Alternative f, c ~ LookupM a r, Aliasing r f)
    => Alias r f -> Pay f -> Species f d -> f a

instance AsSpecies a d => AsSpeciesIf 'Nothing r a d where
  asSpeciesIf _ = asSpecies

instance (Aliasing_ a r, Assoc b d b) => AsSpeciesIf ('Just b) r a d where
  asSpeciesIf alias x s = alias $~ species @b s

type family TypesIn a :: [*]

type Types' r a = Types r a '[]

type Types r a (as :: [*]) = TypesIfElem (Elem a as) r a as

type family Elem a (as :: [*]) :: Bool where
  Elem a (a ': as) = 'True
  Elem a (b ': as) = Elem a as
  Elem a '[] = 'False

type family TypesIfElem (b :: Bool) (r :: [(*, *)]) a (as :: [*]) :: [*] where
  TypesIfElem 'True r a as = as
  TypesIfElem 'False r a as = TypesIfAlias (LookupM a r) r a (a ': as)

type family TypesIfAlias (b :: Maybe *) (r :: [(*, *)]) a (as :: [*]) :: [*] where
  TypesIfAlias 'Nothing r a as = FoldTypes r (TypesIn a) as
  TypesIfAlias ('Just b) r a as = Types r b as

type GTypesIn a = GTypesIn_ (Rep a) '[]

type family GTypesIn_ (f :: * -> *) (as :: [*]) :: [*] where
  GTypesIn_ (K1 i a) as = a ': as
  GTypesIn_ (f :*: g) as = GTypesIn_ f (GTypesIn_ g as)
  GTypesIn_ (f :+: g) as = GTypesIn_ f (GTypesIn_ g as)
  GTypesIn_ (M1 i c f) as = GTypesIn_ f as
  GTypesIn_ U1 as = as
  GTypesIn_ V1 as = as

type family FoldTypes (r :: [(*, *)]) (bs :: [*]) (as :: [*]) :: [*] where
  FoldTypes r '[] as = as
  FoldTypes r (a ': bs) as = Types r a (FoldTypes r bs as)

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
  Duplicate '[] = '[]

asSystemFor
  :: forall a r f d
  .  (Alternative f, Aliasing r f, d ~ Duplicate (Types' r a), AsSpecies' r d d)
  => System_ r f d
asSystemFor = asSystem

type instance TypesIn [a] = '[a]
type instance TypesIn (Maybe a) = '[a]
type instance TypesIn (Either a b) = '[a, b]
type instance TypesIn (a, b) = '[a, b]
type instance TypesIn (a, b, c) = '[a, b, c]
type instance TypesIn (a, b, c, e) = '[a, b, c, e]
type instance TypesIn () = '[]
type instance TypesIn Bool = '[]
type instance TypesIn Ordering = '[]

instance (Assoc a d a, Assoc [a] d [a]) => AsSpecies [a] d where
  asSpecies _ = gspecies

instance Assoc a d a => AsSpecies (Maybe a) d where
  asSpecies _ = gspecies

instance (Assoc a d a, Assoc b d b) => AsSpecies (Either a b) d where
  asSpecies _ = gspecies

instance (Assoc a d a, Assoc b d b) => AsSpecies (a, b) d where
  asSpecies _ = gspecies

instance (Assoc a d a, Assoc b d b, Assoc c d c) => AsSpecies (a, b, c) d where
  asSpecies _ = gspecies

instance (Assoc a d a, Assoc b d b, Assoc c d c, Assoc e d e) => AsSpecies (a, b, c, e) d where
  asSpecies _ = gspecies

instance AsSpecies () d
instance AsSpecies Bool d
instance AsSpecies Ordering d

sizedGeneratorFor
  :: forall a r d m
  .  ( KnownNat (Length d), Assoc a d a, WAlternative Double m
     , d ~ Duplicate (Types' r a)
     , AsSpecies' r d d )
  => FAlias r m
  -> Options
  -> m a
sizedGeneratorFor alias opts = sizedGenerator @_ @a alias (asSystemFor @a) opts
