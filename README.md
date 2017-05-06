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

A combinatorial species `S` is a family of *objects* (trees, lists, graphs...),
with a non-negative `size :: S -> Integer` function, such that there is a
finite number of objects of any given size.

The type of combinatorial species with objects of type `a` is `f a`,
for an abstract `f` which is an instance of `Alternative`.

- `pure a` is the species with a single object `a` with size `0`.

- Given two species `f` and `g`, `liftA2 (,) f g` is the species whose
  objects are pairs `(a, b)` of objects of `f` and `g`, with

    ```haskell
    size (a, b) = size a + size b
    ```

- `empty` is the empty species, with no objects.

- `(Left <$> f) <|> (Right <$> g)` (disjoint sum) is the species whose
  objects are either objects `a` in `f`, or `b` in `g`, with

    ```haskell
    size (Left a) = size a
    size (Right b) = size b
    ```

It is also possible to use `(<*>)` and `(<|>)` with no restrictions,
then there may be duplicate objects which will be counted with multiplicity.

We may define a family of recursive species `y` as a solution (the smallest
by inclusion) of a system `y = F(x, y)`, where `x` is an abstract operation
on species to be interpreted as increasing the size of every object by 1. We
call it `x` by analogy with the corresponding operation on generating
functions; `testing-feat` calls it `pay`.

We use `system` to specify such an `F`.

For instance, consider the following type of binary trees,
consisting of nullary leaves and binary inner nodes:

```haskell
data Tree = Leaf | Node Tree Tree
```

Here is a system for the combinatorial species of `Tree` with the number of
leaves as size.

```haskell
trees :: System '[] '['(Tree, Tree)]
trees = system (\_ x trees ->           -- Note there is shadowing
  (   x (pure Leaf)                     -- Leaves have size 1
  <|> let t = species @Tree trees
      in liftA2 Node t t                -- The subspecies of nodes is a product
                                        -- of the species of trees
  ) /\ none)
```

Not all systems have a solution, even though they may still compile, for
instance, if you forget to use `x`. The following system would assign size 0 to
all trees, of which there are infinitely many. Thus, this does not define a
combinatorial species.

```haskell
trees :: System '[] '['(Tree, Tree)]
trees = system (\_ x trees ->
  (   pure Leaf
  <|> let t = species @Tree trees
      in liftA2 Node t t
  ) /\ none)
```

References
----------

- The core theory of Boltzmann samplers is described in
  [Boltzmann Samplers for the Random Generation of Combinatorial Structures](http://algo.inria.fr/flajolet/Publications/DuFlLoSc04.pdf),
  P. Duchon, P. Flajolet, G. Louchard, G. Schaeffer.

- The numerical evaluation of recursively defined generating functions
  is taken from
  [Boltzmann Oracle for Combinatorial Systems](http://www.dmtcs.org/pdfpapers/dmAI0132.pdf),
  C. Pivoteau, B. Salvy, M. Soria.
