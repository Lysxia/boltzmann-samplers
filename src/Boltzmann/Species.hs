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

  -- Types
  , PreSystem
  , System_
  , F
  ) where

import Boltzmann.Species.System

type System d = forall e. (Equation e, Injection e d) => System_ e d

