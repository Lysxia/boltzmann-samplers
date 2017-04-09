{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Boltzmann.Species
  ( Sized(..)
  , System
  , system
  , lookupSys
  , emptySys
  , (/\)
  , Equation

  --
  , Alternative(..)
  , WAlternative(..)

  -- Types
  , PreSystem
  , System_
  , F
  ) where

import Control.Applicative

import Boltzmann.Species.System

