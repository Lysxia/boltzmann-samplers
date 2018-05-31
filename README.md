Boltzmann samplers [![Hackage](https://img.shields.io/hackage/v/boltzmann-samplers.svg)](https://hackage.haskell.org/package/boltzmann-samplers) [![Build Status](https://travis-ci.org/Lysxia/boltzmann-samplers.svg)](https://travis-ci.org/Lysxia/boltzmann-samplers)
==================

`Boltzmann.Data`
----------------

Define sized random generators for `Data.Data` generic types.

```haskell
    {-# LANGUAGE DeriveDataTypeable #-}

    import Data.Data
    import Test.QuickCheck
    import Boltzmann.Data

    data Term = Lambda Int Term | App Term Term | Var Int
      deriving (Show, Data)

    instance Arbitrary Term where
      arbitrary = sized $ generatorPWith [positiveInts]

    positiveInts :: Alias Gen
    positiveInts =
      alias $ \() -> fmap getPositive arbitrary :: Gen Int

    main = sample (arbitrary :: Gen Term)
```

- Objects of the same size (number of constructors) occur with the same
  probability (see Duchon et al., references below).
- Implements rejection sampling and pointing.
- Works with QuickCheck and MonadRandom, but also similar user-defined monads
  for randomness (just implement `MonadRandomLike`).
- Can be tweaked somewhat with user defined generators.

`Boltzmann.Species`
-------------------

An experimental interface to obtain Boltzmann samplers from an applicative
specification of a combinatorial system.

No documentation (yet).

References
----------

- The core theory of Boltzmann samplers is described in
  [Boltzmann Samplers for the Random Generation of Combinatorial Structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf),
  P. Duchon, P. Flajolet, G. Louchard, G. Schaeffer.

- The numerical evaluation of recursively defined generating functions
  is taken from
  [Boltzmann Oracle for Combinatorial Systems](http://www.dmtcs.org/pdfpapers/dmAI0132.pdf),
  C. Pivoteau, B. Salvy, M. Soria.

See also
--------

- [boltzmann-brain](https://hackage.haskell.org/package/boltzmann-brain-1.3.1.3),
  Boltzmann sampler compiler for combinatorial systems.
