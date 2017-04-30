{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Boltzmann.Species.Generic where

import Control.Applicative
import Boltzmann.Species.System
import Generics.OneLiner
import GHC.Generics
import GHC.TypeLits

import Data.TypeMap.Static

class (KnownNat (Index a d), Lookup a d ~ a) => Assoc' d a
instance (KnownNat (Index a d), Lookup a d ~ a) => Assoc' d a

gspecies
  :: forall f d b
  .  (Alternative f, ADT b, Constraints b (Assoc' d))
  => Species f d -> f b
gspecies s =
  createA (For :: For (Assoc' d)) (species @a s :: forall a. Assoc' d a => f a)
