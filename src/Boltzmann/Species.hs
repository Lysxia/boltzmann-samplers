module Boltzmann.Species
  (
  -- * Combinatorial systems
    System
  , system
  , (/\)
  , emptySys
  , Equation
  , lookupSys
  , Alternative(..)
  , Sized(..)

  -- * Random generators
  , WAlternative(..)

  -- * Options
  , Options(..)
  , sizedOptions
  , singularOptions
  ) where

import Control.Applicative

import Boltzmann.Species.System
import Boltzmann.Options
