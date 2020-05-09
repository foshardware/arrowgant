# arrowgant

Combinators for the `Arrow` type.

## Control.Arrow.Select

### ArrowSelect

The `ArrowSelect` typeclass maps an arrow into any `Traversable`. The simplest instance of this is pure function type `(->)`.

```haskell

class Arrow a => ArrowSelect a where

  select :: Traversable f => a b c -> a (f b) (f c)


instance ArrowSelect (->) where

  select = fmap


```

### ArrowRace

`ArrowRace` provides a generic interface for some process to race against the other. This is intended for use in concurrency context with access to `IO`.  
It is sufficient to implement the more generic combinator `(///)`.

```haskell

infixr 4 ///, \\\

class ArrowChoice a => ArrowRace a where

  (///) :: a b c -> a d e -> a (b, d) (Either c e)

  (\\\) :: a b c -> a b c -> a b c
  a \\\ b = id &&& id >>> a /// b >>> id ||| id

```

## Control.Arrow.Algebraic

The `Algebraic` type takes an algebraic approach to the arrow notation.

```haskell

instance Arrow a => ArrowTransformer Algebraic a where
  lift = Lift

```

Most notably the `mapReduce :: Arrow a => Algebraic a b c -> Algebraic a b c` combinator treats operators `(***)`, `(&&&)`, `first` and `second` as a way to express parallelism.

After using `mapReduce` you can the function `algebraic` to unlift the underlying arrow.

```
algebraic :: Arrow a => Algebraic a b c -> a b c
```

## Control.Arrow.Memo

The most prominent operator in this namespace is `dag`. Its purpose to apply some arrow operation recursively from the bottom up, e.g. installing a dependency tree.  
Given any recursive data type `b` that resembles a directed acyclic graph (DAG), there exists a value of type `DAG k b`, where `k` identifies a node uniquely.

```
data DAG k b = DAG (b -> k) (Lens' b (Map k b))
```

Picture a dependency tree with type `Dependency` that is already resolved and now needs installing. There exists a lens for each dependency to get and set its very own dependencies called `dependencies`. Then the corresponding `dag` value may be defined as follows:

```haskell

dependencyTree :: DAG Identifier Dependency
dependencyTree = DAG (view identifier) dependencies

```

Now for an arrow `(~>)` that allows for side effects necessary for installation and an operator of type `install :: Dependency ~> Dependency`, you may now install the whole dependency tree by calling `dag dependencyTree install :: Dependency ~> Dependency`.



