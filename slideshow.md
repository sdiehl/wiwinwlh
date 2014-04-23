Stephen Diehl (<a class="author" href="https://twitter.com/smdiehl">@smdiehl</a> )

Since I wrote these slides for a little user group talk I gave two years ago they have become a surprisingly
popular reference. I decided to actually turn them into a proper skimmable reference for intermediate level
Haskell topics that don't necessarily have great coverage or that tend be somewhat opaque as to where to get
going, and then aggregate a bunch of the best external resources for diving into those subjects with more
depth. Hopefully it still captures the "no bullshit brain dump" style that seemed to be liked.

The source for all snippets is [available here](https://github.com/sdiehl/wiwinwlh). If there are any errors
or you think of a more illustrative example feel free to submit a pull request.

Cabal
=====

To start a new Haskell project run

```bash
$ cabal init
$ cabal configure
```

A ``.cabal`` file will be created.

Sandboxes ( in cabal > 1.8 ) are self contained environments of Haskell packages.

```bash
$ cabal sandbox init
```

To update the package index from Hackage.

```bash
$ cabal update
```

To install the dependencies for the package:

```bash
$ cabal install --only-dependencies
```

To run the "executable" for a library under the cabal sandbox:

```bash
$ cabal run
$ cabal run <name>
```

To load the "library" into a GHCi shell under the cabal sandbox:

```bash
$ cabal repl
$ cabal repl <name>
```

An example cabal file:

```bash
name:               mylibrary
version:            0.1
cabal-version:      >= 1.10
author:             Paul Atreides 
license:            MIT
license-file:       LICENSE
synopsis:           The code must flow.
category:           Math
tested-with:        GHC
build-type:         Simple
  
library                    
    exposed-modules:
      Library.ExampleModule1
      Library.ExampleModule2

    build-depends: 
      base >= 4 && < 5

    default-language: Haskell2010

    ghc-options: -O2 -Wall -fwarn-tabs

executable "example"
    build-depends: 
        base >= 4 && < 5
    default-language: Haskell2010
    main-is: Main.hs    
```

Using the ``cabal repl`` and ``cabal run`` commands are preferable but sometimes we'd like to perform their
equivalents at the shell, there are several useful aliases that rely on shell directory expansion to find the
package database in the current working directory and launch GHC with the appropriate flags:

```bash
alias ghc-sandbox="ghc -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d"
alias ghci-sandbox="ghci -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d"
alias runhaskell-sandbox="runhaskell -no-user-package-db -package-db .cabal-sandbox/*-packages.conf.d"
```

Courtesy of [Brian McKenna](https://twitter.com/puffnfresh) there is also a zsh script to show the sandbox
status of the current directory in our shell.

```bash
function cabal_sandbox_info() {
    cabal_files=(*.cabal(N))
    if [ $#cabal_files -gt 0 ]; then
        if [ -f cabal.sandbox.config ]; then
            echo "%{$fg[green]%}sandboxed%{$reset_color%}"
        else
            echo "%{$fg[red]%}not sandboxed%{$reset_color%}"
        fi
    fi
}
 
RPROMPT="\$(cabal_sandbox_info) $RPROMPT"
```

GHCi
====

Command    Shortcut   Action
---------  ---------  --------------------------
`:reload`  `:r`       Code reload 
`:type`    `:t`       Type inspection
`:kind`    `:k`       Kind inspection
`:info`    `:i`       Instance inspection
`:print`   `:p`       Print the expression
`:edit`    `:e`       Load file in system editor.

```haskell
λ: :type 3
3 :: Num a => a
```

```haskell
λ: :kind Either
Either :: * -> * -> *
```

```haskell
λ: :info Functor
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  	-- Defined in `GHC.Base'
  ...
```

The current state of the environment can also be queried.

```haskell
λ: :browse
λ: :show bindings
```

A local copy of Hoogle can be installed and used from within GHCi.

```bash
cabal install hoogle
cabal install hlint
```

We can use it by adding a command to our ``~/.ghc/ghci.conf``.

~~~~ {.haskell include="src/ghci.conf"}
~~~~

For editor integration with vim and emacs:

```haskell
cabal install hdevtools
cabal install ghc-mod
```

Exceptions
----------

Debugging uncaught exceptions from bottoms or asynchronous exceptions is very similar to debugging segfaults
with gdb.

```haskell
λ: :set -fbreak-on-exception
λ: :trace main
λ: :hist
λ: :back
```

Bottoms
=======

```haskell
error :: String -> a
undefined :: a
```

The bottom is a singular value that inhabits every type. When evaluated the semantics of Haskell longer
yield a meaningful value. It's usually written as the symbol ⊥ (i.e. the compiler flipping you off ).

An example of a infinite looping term:

```haskell
f :: a
f = let x = x in x
```

The ``undefined`` function is nevertheless extremely practical to accommodate writing incomplete programs and
for debugging.  When combined with the editor tools like hdevtools it can also serve as an ad-hoc type-hole
for debugging.

```haskell
f :: a -> Complicated Type
f = undefined -- write tomorrow, typecheck today!
```

Partial functions from non-exhaustive pattern matching is probably the most common introduction of bottoms.
GHC can be made more vocal about this using the ``-fwarn-incomplete-patterns`` and
``-fwarn-incomplete-uni-patterns`` flags.

```haskell
data F = A | B
case x of 
  A -> ()
```

Is translated into the following GHC Core with the exception inserted for the non-exhaustive patterns.

```haskell
case x of _ {
  A -> ();
  B -> patError "<interactive>:3:11-31|case"
}
```

The same holds with record construction with missing fields, although there's almost never a good reason to
construct a record with missing fields and GHC will warn us.

```haskell
data Foo = Foo { example1 :: Int }
f = Foo {}
```

Again this has an error term put in place by the compiler:

```haskell
Foo (recConError "<interactive>:4:9-12|a")
```

What's not immediately apparent is that they are used extensively throughout the Prelude, some for practical
reasons others for historical reasons. The canonical example is the ``head`` function which as written ``[a]
-> a`` could not be well-typed without the bottom.

~~~~ {.haskell include="src/bottoms.hs"}
~~~~

It's rare to see these partial functions thrown around carelessly in production code and the preferred method
is instead to use the safe variants provided in ``Data.Maybe`` combined with the usual fold functions
``maybe`` and ``either`` or to use pattern matching.

```haskell
listToMaybe        :: [a] -> Maybe a
listToMaybe []     =  Nothing
listToMaybe (a:_)  =  Just a
```

Monads
======

Eightfold Path to Monad Satori
------------------------------

Much ink has been spilled waxing lyrical about the supposed mystique of monads. Instead I suggest a path to
enlightenment:

1. Don't read the monad tutorials.
2. No really, don't read the monad tutorials.
3. Learn about Haskell types.
4. Learn what a typeclass is.
5. Read the [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia).
6. Read the monad definitions.
7. Use monads in real code.
8. Don't write monad-analogy tutorials.

In other words, the only path to understanding monads is to read the fine source, fire up GHC and write some
code. Analogies and metaphors will not lead to understanding.

See: [Monad Tutorial Fallacy](http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/)

Monadic Myths
-------------

The following are all **false**:

* Monads are impure.
* Monads are about effects.
* Monads are about state.
* Monads are about sequencing.
* Monads are about IO.
* Monads are dependent on laziness.
* Monads are a "back-door" in the language to perform side-effects.
* Monads are an embedded imperative language inside Haskell.
* Monads require knowing abstract mathematics.

See: [What a Monad Is Not](http://www.haskell.org/haskellwiki/What_a_Monad_is_not)

Laws
----

Monads are not complicated, the implementation is a typeclass with two functions, ``(>>=)`` pronounced "bind"
and ``return``. Any preconceptions one might have for the word "return" should be discarded, it has an
entirely different meaning.

```haskell
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
```
Together with three laws that all monad instances must satisfy.

**Law 1**

```haskell
return a >>= f ≡ f a
```

**Law 2**

```haskell
m >>= return ≡ m
```

**Law 3**

```haskell
(m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
```

There is an auxiliary function defined in terms of the bind operation that discards its argument.

Also ``(>>)`` in terms of ``(>>=)``:

```haskell
(>>) :: Monad m => m a -> m b -> m b
m >> k = m >>= \_ -> k
```

See: [Monad Laws](http://www.haskell.org/haskellwiki/Monad_laws)

Do Notation
-----------

Monads syntax in Haskell is written in sugared form that is entirely equivalent to just applications of the
monad operations. The desugaring is defined recursively by the rules: 

```haskell
do { a <- f ; m }  ≡  f >>= \a -> m
do { f ; m }       ≡  f >> m
```

So for example:

```haskell
do {
  a <- f ;
  b <- g ;
  c <- h ;
  return (a, b, c)
}

f >>= \a ->
  g >>= \b ->
    h >>= \c ->
      return (a, b, c)
```

In the do-notation the monad laws from above are equivalently written:

**Law 1**

```haskell
  do x <- m
     return x

= do m
```

**Law 2**

```haskell
  do y <- return x
     f y

= do f x
```

**Law 3**

```haskell
  do b <- do a <- m
             f a
     g b

= do a <- m
     b <- f a
     g b

= do a <- m
     do b <- f a
        g b
```

See: [Haskell 2010: Do Expressions](http://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-470003.14)

Maybe
-----

The *Maybe* monad is the simplest first example of a monad instance.

```haskell
data Maybe a = Just a | Nothing
```

```haskell
instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing  >>= k = Nothing

  return = Just
```

```haskell
(Just 3) >>= (\x -> return (x + 1))
-- Just 4

Nothing >>= (\x -> return (x + 1))
-- Nothing

return 4 :: Maybe Int
-- Just 4
```

~~~~ {.haskell include="src/maybe.hs"}
~~~~

List
----

The *List* monad is the second simplest example of a monad instance.

```haskell
instance Monad [] where
  m >>= f   =  concat (map f m)
  return x  =  [x]
```

So for example with:

```haskell
m = [1,2,3,4]
f = \x -> [1,0]
```

The reduction is straightforward:

```haskell
m >>= f
==> [1,2,3,4] >>= \x -> [1,0]
==> concat (map (\x -> [1,0]) [1,2,3,4])
==> concat ([[1,0],[1,0],[1,0],[1,0]])
==> [1,0,1,0,1,0,1,0]
```

The list comprehension syntax in Haskell can be implemented in terms of the list monad.

```haskell
[(x,y) | x <- xs, y <- ys]
```

~~~~ {.haskell include="src/list.hs"}
~~~~

IO
--

A value of type ``IO a`` is a computation which, when performed, does some I/O before returning a value of
type ``a``. Desugaring the IO monad:

```haskell
main :: IO ()
main = do putStrLn "What is your name: "
          name <- getLine
          putStrLn name
```

```haskell
main :: IO ()
main = putStrLn "What is your name:" >>=
       \_    -> getLine >>=
       \name -> putStrLn name
```

```haskell
main :: IO ()
main = putStrLn "What is your name: " >> (getLine >>= (\name -> putStrLn name))
```

See: [Haskell 2010: Basic/Input Output](http://www.haskell.org/onlinereport/haskell2010/haskellch7.html)

Whats the point?
----------------

Consider the non-intuitive fact that we now have a uniform interface for talking about three very different
but foundational ideas for programming: *Failure*, *Collections*, and *Effects*.

Let's write down a new function called ``sequence`` which folds a function ``mcons``, which we can think of as
analogues to the list constructor (i.e. ``(a : b : [])``) except it pulls the two list elements out of of two
monadic values (``p``,``q``) using bind.

```haskell
sequence :: Monad m => [m a] -> m [a] 
sequence = foldr mcons (return [])

mcons :: Monad m => m t -> m t -> m [t]
mcons p q = do
  x <- p
  y <- q
  return [x,y]
```

What does this function mean in terms of each of the monads discussed above?

**Maybe**

Sequencing a list of a ``Maybe`` values allows us to collect the results of a series of computations which can
possibly fail and yield the aggregated values only if they all succeeded. 

```haskell
sequence :: [Maybe a] -> Maybe [a]
```

```haskell
sequence [Just 3, Just 4]
-- Just [3,4]
sequence [Just 3, Just 4, Nothing]
-- Nothing
```

**List**

Since the bind operation for the list monad forms the pairwise list of elements from the two operands, folding
the bind over a list of lists with ``sequence`` implements the general Cartesian product for an arbitrary
number of lists.

```haskell
sequence :: [[a]] -> [[a]]
```

```haskell
sequence [[1,2,3],[10,20,30]]
-- [[1,10],[1,20],[1,30],[2,10],[2,20],[2,30],[3,10],[3,20],[3,30]]
```

**IO**

Sequence takes a list of IO actions, performs them sequence tally, and returns the list of resulting values in
the order sequenced.

```haskell
sequence :: [IO a] -> IO [a]
```

```haskell
sequence [getLine, getLine]
-- a
-- b
-- ["a","b"]
```

So there we have it, three fundamental concepts of computation that are normally defined independently of each
other actually all share this similar structure that can be abstracted out and reused to build higher
abstractions that work for all current and future implementations. If you want a motivating reason for
understanding monads, this is it! This is the essence of what I wish I knew about monads looking back.

See: [Control.Monad](http://hackage.haskell.org/package/base-4.6.0.1/docs/Control-Monad.html#g:4)

mtl and transformers
--------------------

Most of the everyday monads live in either the "mtl" or "transformers" libraries. Of interest to us are:

* Reader
* Writer
* State

See: [transformers](https://github.com/ekmett/transformers/tree/master/Control/Monad/Trans)

Reader Monad
============

```haskell
ask :: Reader r a -> a
asks :: (r -> a) -> Reader r a
local :: (r -> b) -> Reader b a -> Reader r a
runReader :: Reader r a -> r -> a
```

~~~~ {.haskell include="src/reader.hs"}
~~~~

A simple implementation of the Reader monad:

~~~~ {.haskell include="src/reader_impl.hs"}
~~~~

Writer Monad
============

```haskell
tell :: w -> Writer w ()
execWriter :: Writer w a -> w
runWriter  :: Writer w a -> (a, w)
```

~~~~ {.haskell include="src/writer.hs"}
~~~~

An simple implementation of the Writer monad:

~~~~ {.haskell include="src/writer_impl.hs"}
~~~~

This implementation is lazy so some care must be taken that one actually wants only generate a stream of
thunks.  Often this it is desirable to produce a computation which requires a stream of thunks that can pulled
lazily out of the ``runWriter``, but often times the requirement is to produce a finite stream of values that
are forced at the invocation of ``runWriter``. Undesired laziness from Writer is a common source of grief, but
is very remediable.

State Monad
===========

The state monad allows functions within a stateful context to access and modify pass state.

```haskell
runState  :: State s a -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s
```

~~~~ {.haskell include="src/state.hs"}
~~~~

The state monad is often mistakingly as being impure, but it is in fact entirely pure and the same effect
could be achieved by explicitly passing state. An simple implementation of the State monad is only a few
lines:

~~~~ {.haskell include="src/state_impl.hs"}
~~~~

Syntax Extensions 
=================

**Pattern Guards**

```haskell
{-# LANGUAGE PatternGuards #-}

combine env x y
   | Just a <- lookup env x
   , Just b <- lookup env y
   = Just a + b

   | otherwise = Nothing
```

**Multi-way if-expressions**

```haskell
{-# LANGUAGE MultiWayIf #-}

operation x =
  if | x > 100   = 3  
     | x > 10    = 2  
     | x > 1     = 1  
     | otherwise = 0
```

**Lambda Case**

```haskell
{-# LANGUAGE LambdaCase #-}

data Exp a
  = Lam a (Exp a)
  | Var a
  | App (Exp a) (Exp a)

example :: Exp a -> a
example = \case
  Lam a b -> a
  Var a   -> a
  App a b -> example a
```

Laziness
========

Again, a subject on which *much* ink has been spilled. There is an ongoing discussion in the land of Haskell
about the compromises between lazy and strict evaluation, and there are nuanced arguments for having either
paradigm be the default. Haskell takes a hybrid approach and allows strict evaluation when needed and uses
laziness by default. We can always find examples where lazy evaluation exhibits worse behavior than strict
evaluation and vice versa. They both have flaws, and as of yet there isn't a  method that combines only the
best of both worlds.

See: 

* [Oh My Laziness!](http://alpmestan.com/2013/10/02/oh-my-laziness/)
* [Reasoning about Laziness](http://www.slideshare.net/tibbe/reasoning-about-laziness)
* [More Points For Lazy Evaluation](http://augustss.blogspot.hu/2011/05/more-points-for-lazy-evaluation-in.html)

Seq and WHNF
------------

In Haskell evaluation only occurs at outer constructor of case-statements in Core. If we pattern match on a
list we don't implicitly force all values in the list. A element in the list is only evaluated when we
scrutinize it's cons cell.

A term is said to be in *weak head normal-form* if the outermost constructor or lambda cannot be reduced
further.

The ``seq`` function introduces an artificial dependence on the evaluation of order of two terms by requiring
that the first argument be evaluated to WHNF before the evaluation of the second.

```haskell
⊥ `seq` a = ⊥
a `seq` b = b
```

The famous ``foldl`` is well-known to leak space when used carelessly and without several compiler
optimizations applied. The strict ``foldl'`` variant uses seq to overcome this.

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
```

```haskell
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs
```

The extension ``BangPatterns`` allows an alternative syntax to force arguments to functions to be wrapped in
seq.

```haskell
{-# LANGUAGE BangPatterns #-}

sum :: Num a => [a] -> a
sum = go 0
  where
    go !acc (x:xs) = go (acc + x) (go xs)
    go  acc []     = acc
```

Individual fields in records and arguments to constructors can also be explicitly annotated as strict

```haskell
data A = A !Int
```

Deepseq
-------

There are often times when for performance reasons we need to deeply evaluate a data structure to normal form
leaving no terms unevaluated. The ``deepseq`` library performs this task.

```haskell
class NFData a where
  rnf :: a -> ()

deepseq :: NFData a => a -> b -> a
```

```haskell
[1, undefined] `seq` ()
-- ()

[1, undefined] `deepseq` ()
-- Prelude.undefined
```

Text / ByteString
=================

The default Haskell string type is the rather naive list of characters that while perfectly fine for small
identifiers is not well-suited for bulk processing.

```haskell
type String = [Char]
```

With the ``-XOverloadedStrings`` extension string literals can be overloaded without the need for explicit
packing.

```haskell
class IsString a where
  fromString :: String -> a
```

For instance:

```haskell
λ: :type "foo"
"foo" :: [Char]

λ: :set -XOverloadedStrings

λ: :type "foo"
"foo" :: IsString a => a
```

Text
----

A Text type is a packed blob of Unicode characters.


```haskell
pack :: String -> Text
unpack :: Text -> String
```

~~~~ {.haskell include="src/text.hs"}
~~~~

See: [Text](http://hackage.haskell.org/package/text-1.1.0.1/docs/Data-Text.html)


ByteString
----------

ByteStrings are arrays of unboxed chars with either strict or lazy evaluation.

```haskell
pack :: String -> ByteString
unpack :: ByteString -> String
```

~~~~ {.haskell include="src/bytestring.hs"}
~~~~

See: [ByteString](http://hackage.haskell.org/package/bytestring-0.10.4.0/docs/Data-ByteString.html)

Applicatives
============

Like monads Applicatives are an abstract structure for a wide class of computations.

```haskell
pure :: Applicative f => a -> f a
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

As of GHC 7.6, Applicative is defined as:

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```

With the following laws:

```haskell
pure id <*> v = v
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
```

As a rule of thumb, whenever we would use ``m >>= return . f`` what we probably want is an applicative
functor, and not a monad.

~~~~ {.haskell include="src/applicative.hs"}
~~~~

The pattern ``f <$> a <*> b ...`` shows us so frequently that there are a family of functions to lift
applicatives of a fixed number arguments.  This pattern also shows up frequently with monads (``liftM``, ``liftM2``, ``liftM3``).

```haskell
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = f <$> a <*> b <*> c

...
```

See: 

* [Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf)

Monad + Functor Hierarchy
-------------------------

In principle every monad arises out of an applicative functor (and by corollary a functor) but due to
historical reasons Applicative isn't a superclass of the Monad typeclass. A hypothetical fixed Prelude might
have:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
 
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
 
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  ma >>= f = join (fmap f ma)

return :: Applicative m => a -> m a
return = pure

join :: Monad m => m (m a) -> m a
join x = x >>= id
```

RWS Monad
=========

The RWS monad is a combination of the three monads discussed above, the **R**eader, **W**riter, and **S**tate.

```haskell
runReader :: Reader r a -> r -> a
runWriter :: Writer w a -> (a, w)
runState  :: State s a -> s -> (a, s)
```

These three eval functions are now combined into the following functions:

```haskell
runRWS  :: RWS r w s a -> r -> s -> (a, s, w)
execRWS :: RWS r w s a -> r -> s -> (s, w)
evalRWS :: RWS r w s a -> r -> s -> (a, w)
```

~~~~ {.haskell include="src/rws.hs"}
~~~~

The usual caveat about Writer also applies to RWS.

Monad Transformers
==================

Monad transformers allow us to nest monadic computations in a stack with an interface to exchange values
between the levels, called ``lift``. The Monad Transformer Library (mtl) is the common implementation.

```haskell
lift :: (Monad m, MonadTrans t) => m a -> t m a
liftIO :: MonadIO m => IO a -> m a
```

```haskell
class MonadTrans t where
    lift :: Monad m => m a -> t m a

class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id
```

It's useful to remember that transformers compose outside-in but are unrolled inside out.

![](img/transformer_unroll.png)

See: [Monad Transformers: Step-By-Step](http://www.cs.virginia.edu/~wh5a/personal/Transformers.pdf)

Basics
------

The most basic use requires us to use the T-variants of the each of the monad transformers for the outer
layers and to explicit ``lift`` and ``return`` values between each the layers.

~~~~ {.haskell include="src/transformer.hs"}
~~~~

The fundamental limitation of this approach is that ourselves ``lift.lift.lift``ing and
``return.return.return``ing a lot.

Newtype Deriving
----------------

Newtypes let us reference existing types as a new distinct type, with no runtime overhead from boxing.
Newtype wrappers around strings and numeric types can often reduce accidental errors.  Using
``-XGeneralizedNewtypeDeriving`` we can recover the functionality of instances of the underlying type.


~~~~ {.haskell include="src/newtype.hs"}
~~~~

```haskell
Couldn't match type `Double' with `Velocity'
Expected type: Velocity
  Actual type: Position
In the second argument of `(+)', namely `x'
In the expression: v + x
```

Using newtype deriving with typeclasses we can produce flattened transformer types that don't require explicit
lifting the transform stack. For example a little stack machine the Reader Writer and State monads.

~~~~ {.haskell include="src/newtype_deriving.hs"}
~~~~

Error Handling
==============

Control.Exception
-----------------

The low-level (and most dangerous) way to handle errors is to use the ``throw`` and ``catch`` functions which
allow you to throw extensible extensions in pure code but catch the resulting exception within IO.  Of
specific note is that return value of the ``throw`` inhabits all types. There's no reason to use this for
custom code that doesn't use low-level system operations.

```haskell
throw :: Exception e => e -> a
catch :: Exception e => IO a -> (e -> IO a) -> IO a
try :: Exception e => IO a -> IO (Either e a)
evaluate :: a -> IO a
```

~~~~ {.haskell include="src/ioexception.hs"}
~~~~

Exceptions
----------

The problem with the previous approach is having to rely on GHC's asynchronous exception handling inside of IO
to handle basic operations. The ``exceptions`` provides the same API as ``Control.Exception`` but loosens the
dependency on IO.

~~~~ {.haskell include="src/exceptions.hs"}
~~~~

See: [exceptions](http://hackage.haskell.org/package/exceptions)

Either
------

The instance of the Either monad is simple, note the bias toward Left when binding.

~~~~ {.haskell include="src/either_impl.hs"}
~~~~

The silly example one always sees is writing safe division function that fails out with a Left value when a
division by zero happens and holds the resulting value in Right otherwise.

~~~~ {.haskell include="src/either.hs"}
~~~~

This is admittedly pretty stupid but captures the essence of why Either/EitherT is an suitable monad for
exception handling.

ErrorT
------

Another slightly clumsy method is to use the ``ErrorT`` transformer composed with an Identity and unrolling
into an ``Either Exception a``. This method is simple but doesn't compose well depending on the situation and
interaction with IO.

~~~~ {.haskell include="src/errors.hs"}
~~~~

EitherT and errors
------------------

```haskell
newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}
  	-- Defined in `Control.Monad.Trans.Either'
```

```haskell
runEitherT :: EitherT e m a -> m (Either e a)
tryIO :: MonadIO m => IO a -> EitherT IOException m a

throwT  :: Monad m => e -> EitherT e m r
catchT  :: Monad m => EitherT a m r -> (a -> EitherT b m r) -> EitherT b m r
handleT :: Monad m => (a -> EitherT b m r) -> EitherT a m r -> EitherT b m
```

The ideal monad to use is simply the ``EitherT`` monad which we'd like to be able to use an with an API
similar to ``ErrorT`` and be able to gracefully handle exceptions when underlying monad is IO. Nothing yet
quite gives us all this out of the box.

For example suppose we wanted to use ``read`` to attempt to read a positive integer from stdin. There are two
failure modes and two failure cases here, one for a parse error which fails with an error from
``Prelude.readIO``  and one for a non-positive integer which fails with a custom exception after a check. We'd
like to be unify both cases in the same transformer.

Enter the ``safe`` and ``errors`` package which through a little re-export magic make life with ``EitherT``
more pleasant. The safe library provides a variety of safer variants of the standard prelude functions that
handle failures as Maybe values, explicitly passed default values, or more informative exception "notes".
While the errors library reexports the safe Maybe functions and hoists them up into the ``EitherT`` monad
providing a family of ``try`` prefixed functions that perform actions and fail with an exception argument.

```haskell
Safe.headMay :: [a] -> Maybe a
hoistEither :: Monad m => Either e a -> EitherT e m a
hoistMaybe :: Monad m => Maybe b -> MaybeT m b
```

```haskell
-- Exception handling equivalent of `read`
tryRead :: (Monad m, Read a) => e -> String -> EitherT e m a

-- Exception handling equivelant of `head`
tryHead :: Monad m => e -> [a] -> EitherT e m a

-- Exception handling equivelant of `(!!)`
tryAt :: Monad m => e -> [a] -> Int -> EitherT e m a
```

Putting this all together we have pretty close to the ideal monad for error handling.

~~~~ {.haskell include="src/eithert.hs"}
~~~~

See:

* [Error Handling Simplified](http://www.haskellforall.com/2012/07/errors-10-simplified-error-handling.html)
* [Safe](http://hackage.haskell.org/package/safe)

ST Monad
========

The ST monad models "threads" of stateful computations which can manipulate mutable references but are
restricted to only return pure values when evaluated and are statically confined to the ST monad of a ``s``
thread.

```haskell
runST :: (forall s. ST s a) -> a
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
```

~~~~ {.haskell include="src/st.hs"}
~~~~

Using the ST monad we can create a new class of efficient purely functional data structures that use mutable
references.

Foldable / Traversable
======================

If coming from an imperative background retraining one's self to think about iteration in terms of maps,
folds, and scans can be challenging but the end result is better compositionally and code-reuse.

```haskell
-- pseudocode
foldr f z [a...] = f a (f b ( ... (f y z) ... )) 
foldl f z [a...] = f ... (f (f z a) b) ... y 
```

A foldable instance allows us to apply functions to data types of monoidal values that collapse the
structure using some logic over ``mappend``.

A traversable instance allows us to apply functions to data types that walk the structure left-to-right within
an applicative context.

```haskell
class (Functor f, Foldable f) => Traversable f where
  traverse :: Applicative g => f (g a) -> g (f a)

class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m
```

The names exported by foldable quite often conflict with ones defined in the Prelude, either import them
qualified or just disable the Prelude. The operations in the Foldable all specialize to the same behave the
same as the ones Prelude for List types.

~~~~ {.haskell include="src/foldable_traversable.hs"}
~~~~

The instances we defined above can also be automatically derived by GHC using several language extensions, the
results are identical to the hand-written versions above.

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

data Tree a = Node a [Tree a]
  deriving (Show, Functor, Foldable, Traversable)
```

Prelude Legacy
--------------

The instances of Foldable for the list type often conflict with the monomorphic versiosn in the Prelude which
are left in for historical reasons. So often times it is desirable to explicitly mask these functions from
implicit import and force the use of Foldable and Traversable instead:

```haskell
import  Data.List hiding ( 
    all , and , any , concat , concatMap , elem , filter ,
    find , foldl , foldl' , foldl1 , foldr , foldr1 ,
    mapAccumL , mapAccumR , maximum , maximumBy , minimum , 
    minimumBy , notElem , or , product , sum )

import Control.Monad hiding ( 
    forM , forM_ , mapM , mapM_ , msum , sequence , sequence_ )
```

The nuclear option is to exclude the entire prelude except by explicit qualified use.

```haskell
import qualified Prelude as P
```

Free Monads
===========

```haskell
Pure :: a -> Free f a
Free :: f (Free f a) -> Free f a

liftF :: (Functor f, MonadFree f m) => f a -> m a
retract :: Monad f => Free f a -> f a
```

Free monads are monads which instead of having a ``join`` operation that combines computations, instead forms
composite computations from application of a functor.

```haskell
join :: Monad m => m (m a) -> m a
wrap :: MonadFree f m => f (m a) -> m a
```

One of the best examples is the Partiality monad which models computations which can diverge. Haskell allows
unbounded recursion, but for example we can create a free monad from the ``Maybe`` functor which when can be
used to fix the call-depth of, for example, the [Ackermann
functions](https://en.wikipedia.org/wiki/Ackermann_function.)

~~~~ {.haskell include="src/partiality.hs"}
~~~~

The other common use for free monads to build embedded domain languages to describe computations. We can model
a subset of the IO monad by building up a pure description of the computation inside of the IOFree monad
and then using the free monad to encode the translation to an effectful IO computation.

~~~~ {.haskell include="src/free_dsl.hs"}
~~~~


An implementation such as the one found in [free](http://hackage.haskell.org/package/free) might look like the
following:

~~~~ {.haskell include="src/free_impl.hs"}
~~~~

See: 

* [I/O is not a Monad](http://r6.ca/blog/20110520T220201Z.html)

GADTs
=====

GADTs are an extension to algebraic datatypes that allow us to qualify the constructors to datatypes with type
equality constraints, allowing a class of types that are not expressible using vanilla ADTs.

For consider the data type, we have a term in which we ``Succ`` which takes a ``Term`` parameterized by ``a``
which span all types. Problems arise between the clash whether (``a ~ Bool``) or (``a ~ Int``) when trying to
write the evaluator.

```haskell
data Term a
  = Lit a
  | Succ (Term a)
  | IsZero (Term a)

eval (Lit i)      = i
eval (Succ t)     = 1 + eval t
eval (IsZero i)   = eval i == 0
```

And we have:

```haskell
-- This is a valid type.
failure = Succ ( Lit True )
```
Using a GADT we can express the type invariants for our language (i.e. only type-safe expressions are
representable). Pattern matching on this GADTs then carries type equality constraints without the need for
explicit tags.

~~~~ {.haskell include="src/gadt.hs"}
~~~~

This time around:

```haskell
-- This is rejected at compile-time.
failure = Succ ( Lit True )
```

Explicit equality constraints can be added to a function's context with ``-XGADTs`` enabled.

```haskell
f :: (a ~ b) => a -> b -> (a,b)
f x y = (x,y)
```

Kind Signatures
---------------

Recall that the kind in Haskell's type system the "type of the types" or *kinds* is the type system consisting
the single kind ``*`` and an arrow kind ``->``.

```haskell
κ : *
  | κ -> κ 
```

```haskell
Int :: *
Maybe :: * -> *
Either :: * -> * -> *
```

On top of default GADT declaration we can also constrain the parameters of the GADT to specific kinds. For
basic usage Haskell's kind inference can deduce this reasonably well, but combined with some other type system
extensions this becomes essential.

~~~~ {.haskell include="src/kindsignatures.hs"}
~~~~

Equality
--------

With a richer language for datatypes we can express terms that witness the relationship between terms in the
constructors, for example equality:

~~~~ {.haskell include="src/equal.hs"}
~~~~

HOAS
====

Higher Order Abstract Syntax (*HOAS*) is a technique for encoding the lambda calculus that exploits the
function type of the host language ( i.e. Haskell ) to give us capture-avoiding substitution in our custom
language by exploiting Haskell's implementation.

~~~~ {.haskell include="src/hoas.hs"}
~~~~

There is no however no safeguard preventing us from lifting Haskell functions which do not encode meaningful
lambda calculus expression. For example:

```haskell
Lam (\x -> let x = x in x )
```

Pretty printing HOAS encoded terms can also be quite complicated since the body of the function is under a
Haskell lambda binder.

Final Interpreters
==================

Using typeclasses we can implement a *final interpreter* which models a set of extensible terms using
functions bound to typeclasses rather than data constructors. Instances of the typeclass form interpreters
over these terms.

For example we can write a small language that includes basic arithmetic, and then retroactively extend our
expression language with a multiplication operator without changing the base. At the same time our logic for
our evaluator and pretty-printing interpreters remain invariant under the addition of new elements.

~~~~ {.haskell include="src/fext.hs"}
~~~~

Finally Tagless
---------------

Writing an evaluator for the lambda calculus can likewise also be modeled with a final interpeter and a
Identity functor.

~~~~ {.haskell include="src/final.hs"}
~~~~

The [esqueleto](http://hackage.haskell.org/package/esqueleto) library uses this approach internally to build a
embedded domain language for describing SQL queries.

See: 

* [Typed Tagless Interpretations and Typed Compilation](http://okmij.org/ftp/tagless-final/)

Initial Algebras
================

The *initial algebra* approach differs from the final interpreter approach in that we now represent our terms
as algebraic datatypes and the interpreter implements recursion and evaluation occurs through pattern
matching.

Algebra of Datatypes
--------------------

The usual hand-wavy of describing algebraic datatypes is to indicate the how natural correspondence between
sum types, product types, and polynomial expressions arises.

```haskell
data Void                       0
data Unit     = Unit            1
data Sum a b  = Inl a | Inr b   a + b
data Prod a b = Prod a b        a * b
type (->) a b = a -> b          b ^ a
```

```haskell
-- 1 + A
data Maybe a = Nothing | Just a
```

Recursive types are modeled as the infinite series of these terms.

```haskell
-- μX. 1 + X
data Nat a = Z | S Nat

-- μX. 1 + A * X
data List a = Nil | Cons a (List a)
List a = μ a. 1 + a * (List a) 
       = 1 + a + a^2 + a^3 + a^4 ...
```

```haskell
-- μX. A + A*X*X
data Tree a f = Leaf a | Tree a f f
```

See:

* [Species and Functors and Types, Oh My!](http://www.cis.upenn.edu/~byorgey/papers/species-pearl.pdf)

F-Algebras
-----------

```haskell
type Algebra f a = f a -> a
type Coalgebra f a = a -> f a
newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
ana  :: Functor f => Coalgebra f a -> a -> Fix f
hylo :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
```

In Haskell an F-algebra in a functor ``f a`` together with function ``f a -> a``. A colagebra reverses the
function. For a functor ``f`` we can form it's recursive unrolling using the ``Fix`` newtype wrapper.

```haskell
Fix f = f (f (f (f (f (f ( ... ))))))
```

```haskell
newtype Fix f = Fix { unFix :: f (Fix f) }

Fix :: f (Fix f) -> Fix f
unFix :: Fix f -> f (Fix f)
```

In this form we can write down a generalized fold/unfold function that are datatype generic and written purely
in terms of the recursing under the functor.

```haskell
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
```

We call these functions *catamorphisms* and *anamorphisms*. Notice especially that the types of thees two
functions simply reverse the direction of arrows. Interpreted in another way they transform an
algebra/colaglebra which defines a flat structure-preserving mapping between ``Fix f`` ``f`` into a function
which either rolls or unrolls the fixpoint. What is particularly nice about this approach is that the
recursion is abstracted away inside the functor definition and we are free to just implement the flat
transformation logic!

For example a construction of the natural numbers in this form:

~~~~ {.haskell include="src/initial.hs"}
~~~~

Or for example an interpreter for a small expression language that depends on a scoping dictionary.

~~~~ {.haskell include="src/initial_interpreter.hs"}
~~~~

What's especially nice about this approach is how naturally catamorphisms compose into efficient
transformations lending itself to write extensible interpreters.

```haskell
compose :: Functor f => (f (Fix f) -> c) -> (a -> Fix f) -> a -> c
compose f g = f . unFix . g
```

See: 

* [recursion-schemes](http://hackage.haskell.org/package/recursion-schemes)
* [Understanding F-Algebras](https://www.fpcomplete.com/user/bartosz/understanding-algebras)

QuickCheck
==========

Probably the most famous Haskell library, QuickCheck is a testing framework for generating large random tests
for arbitrary functions automatically based on the types of their arguments.
  
```haskell
quickCheck :: Testable prop => prop -> IO ()
(==>) :: Testable prop => Bool -> prop -> Property
forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
choose :: Random a => (a, a) -> Gen a
```

~~~~ {.haskell include="src/qcheck.hs"}
~~~~

```bash
$ runhaskell qcheck.hs
*** Failed! Falsifiable (after 3 tests and 4 shrinks):    
[0]
[1]

$ runhaskell qcheck.hs
+++ OK, passed 1000 tests.
```

The test data generator can be extended with custom types and refined with predicates that restrict the domain
of cases to test.

~~~~ {.haskell include="src/arbitrary.hs"}
~~~~

See:

* [QuickCheck: An Automatic Testing Tool for Haskell](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)

SmallCheck
==========

Like QuickCheck, SmallCheck is a property testing system but instead of producing random arbitrary test data
it instead enumerates a deterministic series of test data to a fixed depth.

```haskell
smallCheck :: Testable IO a => Depth -> a -> IO ()
list :: Depth -> Series Identity a -> [a]
sample' :: Gen a -> IO [a]
```

```haskell
λ: list 3 series :: [Int]
[0,1,-1,2,-2,3,-3]

λ: list 3 series :: [Double]
[0.0,1.0,-1.0,2.0,0.5,-2.0,4.0,0.25,-0.5,-4.0,-0.25]

λ: list 3 series :: [(Int, String)]
[(0,""),(1,""),(0,"a"),(-1,""),(0,"b"),(1,"a"),(2,""),(1,"b"),(-1,"a"),(-2,""),(-1,"b"),(2,"a"),(-2,"a"),(2,"b"),(-2,"b")]
```

It is useful for *all* possible inputs of a program up to some depth.

~~~~ {.haskell include="src/smallcheck.hs"}
~~~~

```haskell
$ runhaskell smallcheck.hs
Testing distributivity...
Completed 132651 tests without failure.

Testing Cauchy-Schwarz...
Completed 27556 tests without failure.

Testing invalid Cauchy-Schwarz...
Failed test no. 349.
there exist [1.0] [0.5] such that
  condition is false
```

Series
------

Just like for QuickCheck we can implement series instances for our custom datatypes. For example there is no
default instance for Vector, so let's implement one:

~~~~ {.haskell include="src/smallcheck_series.hs"}
~~~~

SmallCheck can also use Generics to derive Serial instances, for example to enumerate all trees of a certain
depth we might use:

~~~~ {.haskell include="src/smallcheck_tree.hs"}
~~~~

Printer Combinators
===================

Pretty printer combinators compose logic to print strings.

              Combinators   
-----------   ------------
``<>``        Concatenation
``<+>``       Spaced concatenation
``char``      Renders a character as a ``Doc``
``text``      Renders a string as a ``Doc``

~~~~ {.haskell include="src/pretty.hs"}
~~~~

The pretty printed form of the ``k`` combinator:

```haskell
\f g x . (f (g x))
```

The ``Text.Show.Pretty`` library can be used to pretty print nested data structures in a more human readable
form for any type that implements ``Show``.  For example a dump of the structure for the AST of SK combinator
with ``ppShow``. 

```haskell
App
  (Lam
     "f" (Lam "g" (Lam "x" (App (Var "f") (App (Var "g") (Var "x"))))))
  (Lam "x" (Lam "y" (Var "x")))
```

See:

* [The Design of a Pretty-printing Library](http://belle.sourceforge.net/doc/hughes95design.pdf)

Vector
======

Vectors are high performance single dimensional arrays that come come in six variants, two for each of the
following types of a mutable and an immutable variant.

* Data.Vector
* Data.Vector.Storable
* Data.Vector.Unboxed

The most notable feature of vectors is constant time memory access with (``(!)``) as well as variety of
efficient map, fold and scan operations on top of a fusion framework that generates surprisingly optimal code.

```haskell
fromList :: [a] -> Vector a
toList :: Vector a -> [a]
(!) :: Vector a -> Int -> a
map :: (a -> b) -> Vector a -> Vector b
foldl :: (a -> b -> a) -> a -> Vector b -> a
scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
iterateN :: Int -> (a -> a) -> a -> Vector a
```

~~~~ {.haskell include="src/vector.hs"}
~~~~

See:

* [Numerical Haskell: A Vector Tutorial](http://www.haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial)

Mutable
-------

```haskell
freeze :: MVector (PrimState m) a -> m (Vector a)
thaw :: Vector a -> MVector (PrimState m) a
```

Within the IO monad we can perform arbitrary read and writes on the mutable vector with constant time reads
and writes. When needed a static Vector can be created to/from the ``MVector`` using the freeze/thaw
functions.


~~~~ {.haskell include="src/vector_mutable.hs"}
~~~~

Type Families
=============

Multi-parameter type classes
----------------------------

Resolution of vanilla Haskell 98 typeclasses proceeds via very simple context reduction that minimizes
interdependency between predicates, resolves superclases, and reduces the types to head normal form. For
example:

```haskell
(Eq [a], Ord [a]) => [a]
==> Ord a => [a]
```

If a single parameter typeclass expresses a property of a type ( i.e. it's in a class or not in class ) then a
multiparamater typeclass expresses relationships between types. For example whether if we wanted to express
the relation a type can be converted to another type we might use a class like:

~~~~ {.haskell include="src/mparam.hs"}
~~~~

Of course now our instances for ``Convertible Int`` are not unique anymore, so there no longer exists a nice
procedure for determining the inferred type of ``b`` from just ``a``. To remedy this let's add a functional
dependency ``a -> b``, which says tells GHC that an instance ``a`` uniquely determines the instance that b can
be.  So we'll see that our two instances relating ``Int`` to both ``Integer`` and ``Char`` conflict.

~~~~ {.haskell include="src/mparam_fun.hs"}
~~~~

```haskell
Functional dependencies conflict between instance declarations:
  instance Convertible Int Integer
  instance Convertible Int Char
```

Now there's a simpler procedure for determening instances uniquely and multiparameter typeclasses become more
usable and inferable again.

```haskell
λ: convert (42 :: Int)
'42'
λ: convert '*'
42
```

Now let's make things not so simple. Turning on ``UndecidableInstances`` loosens the constraint on context
reduction can only allow constraints of the class to become structural smaller than it's head. As a result
implicit computation can now occur *within in the type class instance search*. Combined with a type-level
representation of Peano numbers we find that we can encode basic arithmetic at the type-level.

~~~~ {.haskell include="src/fundeps.hs"}
~~~~

If the typeclass contexts look similar to Prolog you're not wrong, if you read the contexts qualifier
``(=>)`` backwards as backwards turnstiles ``:-`` then it's precisely the same equations.

```prolog
add(0, A, A).
add(s(A), B, s(C)) :- add(A, B, C).

pred(0, 0).
pred(S(A), A).
```

This is kind of abusing typeclasses and if used carelessly it can fail to terminate or overflow at
compile-time. ``UndecidableInstances`` shouldn't be turned on without careful forethought about what it
implies.

```haskell
<interactive>:1:1:
    Context reduction stack overflow; size = 201
```

Type Families
-------------

Type famiiles allows us to write families functions in the type domain which take types as arguments which can
yield either types or values indexed on their type arguments. Type families come in two varieties: **data
families** and **type synonym families**.

* "type family" are named function on types
* "data family" are type-indexed data types

First let's look at *type synonym families*, there are two equivalent syntactic ways of constructing them.
Either as *associated* type families declared within a typeclass or as standalone declarations at the
toplevel. The following are semantically equivalent:

```haskell
type family Foo a
type instance Foo Int = ...
```

```haskell
class C a where
   type Foo a

instance C Int where
   type Foo Int = ...
```

Using the same example we used for multiparamater + functional dependencies illustration we see that there is
a direct translation between the type family approach and functional dependencies. These two approaches have
the same expressive power.

```haskell
{-# LANGUAGE TypeFamilies #-}

import Data.Char

type family Rep a
type instance Rep Int = Char
type instance Rep Char = Int

class Convertible a where
  convert :: a -> Rep a

instance Convertible Int where
  convert = chr

instance Convertible Char where
  convert = ord
```

An associated type family can be queried using the ``:kind!`` command in GHCi.

```haskell
λ: :kind! Rep Int
Rep Int :: *
= Char
λ: :kind! Rep Char
Rep Char :: *
= Int
```

*Data families* on the other hand allow us to create new type parameterized data constructors. Normally we can
only define typeclases functions whose behavior results in a unform result which is purely a result of the
typeclasses arguments. With data families we can allow specialized behavior indexed on the type. For example
if we wanted to create more complicated vector structures ( bitmasked vectors, vectors of tuples, ... ) that
exposed a uniform API but internally handled the differences in their data layout we can use data families to
accomplish this:

~~~~ {.haskell include="src/datafamily.hs"}
~~~~

Proofs
------

One of most deep results in computer science, the [Curry–Howard
correspondence](https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence), is the relation that
logical propositions can be modeled by types and instantiating those types constitutes proofs of these
propositions. In dependently typed languages we can exploit this result to it's full extent, in Haskell we
don't have the strength that dependent types provide but can still prove ( for a suffficently lax definition
of the word "prove" ) trivial results. For example, now we can model a type level function for addition and
provide a small proof that zero is an additive identity.

~~~~ {.haskell include="src/family_nat.hs"}
~~~~

Using the the ``TypeOperators`` extension we can also write use infix notation at the type-level.

```haskell
data a :=: b where
  Refl :: a :=: a

cong :: a :=: b -> (f a) :=: (f b)
cong Refl = Refl

type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Zero     :+ m = m
type instance (Succ n) :+ m = Succ (n :+ m)

assoc :: forall m n. Nat m -> Nat n -> Eql (m :+ (S Z) :+ n) (S Z :+ m :+ n)
assoc Zero n = Refl
assoc (Succ m) n = cong (assoc m n)
```

HLists
------

A heterogeneous list is a cons list whose type statically encodes the ordered types of of it's values.

~~~~ {.haskell include="src/hlist.hs"}
~~~~

Constraint Kinds
----------------

The kind of a type family permits arbitrary kinds, but of particular interst in the Constraint kind which is
enabled with the ``-XConstraintKinds`` extension. Using this we can use typeclass constraints as first class
values which can naturally be indexed with the type family.

For a contrived example if we wanted to create a generic ``Sized`` class that carried with it constraints on
the elements of the container in question we could achieve this quite simply using type families.

~~~~ {.haskell include="src/constraintkinds.hs"}
~~~~

One use-case of this is to capture the typeclass dictionary constrained by a function and reify that as a
value.

~~~~ {.haskell include="src/dict.hs"}
~~~~

The empty constraint set is denoted  ``() :: Constraint``.

DataKinds / PolyKinds
=====================

Kind Polymorphism
-----------------

The regular value level function which takes a function and applies it to an argument is generalized in the
usual Hindley-Milner way.

```haskell
app :: forall a b. (a -> b) -> a -> b
app f a = f a
```

But when we do the same thing at the type-level we see we loose information about the polymorphism of the
constructor applied.

```haskell
-- TApp :: (* -> *) -> * -> *
data TApp f a = MkTApp (f a)
```

Turning on PolyKinds allows parametric polymorphism at the kind level as well.

```haskell
-- Default:   (* -> *) -> * -> *
-- PolyKinds: (k -> *) -> k -> *
data TApp f a = MkTApp (f a)

-- Default:   ((* -> *) -> (* -> *)) -> (* -> *)
-- PolyKinds: ((k -> *) -> (k -> *)) -> (k -> *)
data Mu f a = Roll (f (Mu f) a)

-- Default:   * -> *
-- PolyKinds: k -> *
data Proxy a = Proxy
```

Using PolyKinds with the ``Proxy`` type allows us to write down type class functions which over constructors
of arbitrary kind arity.

~~~~ {.haskell include="src/kindpoly.hs"}
~~~~

Data Kinds
----------

The ``-XDataKinds`` extension allows us to use refer to constructors at the value level and the type level.
Consider a simple sum type:

```haskell
data S a b = L a | R b

-- S :: * -> * -> *
-- L :: a -> S a b
-- R :: b -> S a b
```

With the extension enabled we see that we our type constructors are now automatically promoted so that ``L``
or ``R`` can be viewed as both a data constructor of the type ``S`` or as the type ``L`` with kind ``S``.

```haskell
{-# LANGUAGE DataKinds #-}

data S a b = L a | R b

-- S :: * -> * -> *
-- L :: * -> S * *
-- R :: * -> S * *
```

Promoted data constructors can referred to in type signatures by prefixing them with a single quote.  Also of
importance is that these promoted constructors are not exported with a module by default, but type synonym
instances can be created using this notation.

```haskell
data Foo = Bar | Baz
type Bar = 'Bar
type Baz = 'Baz
```

Of interest is that we have access to GHC's type level natural literals:

```haskell
λ: :kind 3
3 :: Nat
λ: :kind (3 + 4)
(3 + 4) :: Nat
λ: :kind (3 <= 4)
(3 <= 4) :: Constraint
```

Using this new structure we can create a ``Vec`` type which is parameterized by it's length as well as it's
element type now that we have a kind language rich enough to encode the successor type in the kind signature
of the generalized algebraic datatype. 

~~~~ {.haskell include="src/datakinds.hs"}
~~~~

So now if we try to zip two ``Vec`` types with the wrong shape then we get a error at compile-time about the
off-by-one error. We've just made a entire class of invalid unrepresentable.

```haskell
example2 = zipVec vec4 vec5
-- Couldn't match type 'S 'Z with 'Z
-- Expected type: Vec Four Int
--   Actual type: Vec Five Int
```

The same technique we can use to create a container which is statically indexed by a empty or non-empty flag,
such that if we try to take the head of a empty list we'll get a compile-time error, or stated equivalently we
have an obligation to prove to the compiler that the argument we hand to the head function is non-empty.

~~~~ {.haskell include="src/nonempty.hs"}
~~~~

```haskell
Couldn't match type None with Many
Expected type: List Many Int
  Actual type: List None Int
```

GHC's type literals can also be used in place of explicit Peano arithmetic, although GHC is very conservative
about performing reduction.

~~~~ {.haskell include="src/typenat.hs"}
~~~~

So we've just expressed the relationship between the type of a data structure based on it's values and
function which can be constrained at compile-time based on these properties. Let that sink in for a moment, go
take some mind-altering substances, and then go check out
[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php).

See:

* [Giving Haskell a Promotion](https://research.microsoft.com/en-us/people/dimitris/fc-kind-poly.pdf)
* [Faking It: Simulating Dependent Types in Haskell](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.22.2636&rep=rep1&type=pdf)

Generics
========

Haskell has several techniques for automatic generation of type classes for a variety of tasks that consist
largely of boilerplate code generation such as:

* Pretty Printing
* Equality
* Serialization
* Ordering
* Traversal

Typeable
--------

The ``Typeable`` class be used to create runtime type information for arbitrary types.

```haskell
typeOf :: Typeable a => a -> TypeRep
```

~~~~ {.haskell include="src/typeable.hs"}
~~~~

Using the Typeable instance allows us to write down a type safe cast function which can safely use
``unsafeCast`` and provide evidence that the resulting type matches the input.

```haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
cast x
    | typeOf x == typeOf ret = Just ret
    | otherwise = Nothing
    where
    ret = unsafeCast x
```

Of historical note is that writing our own Typeable classes is currently possible of GHC 7.6 but allows us to
introduce dangerous behavior that can cause crashes, and shouldn't be done except by GHC itself.

Dynamic
-------

Since we have a way of querying runtime type information we can use this machinery to implement a ``Dynamic``
type. This allows us to box up any monotype into a uniform type that can be passed to any function taking a
Dynamic type which can then unpack the underlying value in a type-safe way.

```haskell
toDyn :: Typeable a => a -> Dynamic
fromDyn :: Typeable a => Dynamic -> a -> a
fromDynamic :: Typeable a => Dynamic -> Maybe a
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

~~~~ {.haskell include="src/dynamic.hs"}
~~~~

Data
----

Just as Typeable let's create runtime type information where needed, the Data class allows us to reflect
information about the structure of datatypes to runtime as needed.

```haskell
class Typeable a => Data a where
  gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)
          -> (forall g. g -> c g)
          -> a
          -> c a

  gunfold :: (forall b r. Data b => c (b -> r) -> c r)
          -> (forall r. r -> c r)
          -> Constr
          -> c a

  toConstr :: a -> Constr
  dataTypeOf :: a -> DataType
  gmapQl :: (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
```

The types for ``gfoldl`` and ``gunfold`` are a little intimidating ( and depend on ``Rank2Types`` ), the best
way to understand is to look at some examples. First the most trivial case a simple sum type ``Animal`` would
produce the follow the following code:

```haskell
data Animal = Cat | Dog deriving Typeable
```

```haskell
instance Data Animal where
  gfoldl k z Cat = z Cat
  gfoldl k z Dog = z Dog

  gunfold k z c
    = case constrIndex c of
        1 -> z Cat
        2 -> z Dog 

  toConstr Cat = cCat
  toConstr Dog = cDog

  dataTypeOf _ = tAnimal

tAnimal :: DataType
tAnimal = mkDataType "Main.Animal" [cCat, cDog]

cCat :: Constr
cCat = mkConstr tAnimal "Cat" [] Prefix

cDog :: Constr
cDog = mkConstr tAnimal "Dog" [] Prefix
```

For a type with non-empty containers we get something a little more interesting. Consider the list type:

```haskell
instance Data a => Data [a] where
  gfoldl _ z []     = z []
  gfoldl k z (x:xs) = z (:) `k` x `k` xs

  toConstr []    = nilConstr
  toConstr (_:_) = consConstr

  gunfold k z c 
    = case constrIndex c of
        1 -> z []
        2 -> k (k (z (:)))

  dataTypeOf _ = listDataType

nilConstr :: Constr
nilConstr = mkConstr listDataType "[]" [] Prefix

consConstr :: Constr
consConstr = mkConstr listDataType "(:)" [] Infix

listDataType :: DataType
listDataType = mkDataType "Prelude.[]" [nilConstr,consConstr]
```

Looking at ``gfoldl`` we see the Data has an implementation of a function for us to walk an applicative over
the elements of the constructor by applying a function ``k`` over each element and applying ``z`` at the
spine. For example look at the instance for a 2-tuple as well:


```haskell
instance (Data a, Data b) => Data (a,b) where
  gfoldl k z (a,b) = k (,) `f` a `f` b

  toConstr (_,_) = tuple2Constr

  gunfold k z c  
    = case constrIndex c of
      1 -> k (k (z (,)))

  dataTypeOf _  = tuple2DataType

tuple2Constr :: Constr
tuple2Constr = mkConstr tuple2DataType "(,)" [] Infix

tuple2DataType :: DataType
tuple2DataType = mkDataType "Prelude.(,)" [tuple2Constr]
```

This is pretty cool, now within the same typeclass we have a generic way to introspect any ``Data`` instance
and writing logic that depends on the structure and types of its subterms. We can now write a function which
allow us to traverse an arbitrary instance Data and twiddle values based on pattern matching on the runtime
types. So let's write down a function ``over`` which increments a ``Value`` type for both for n-tuples and
lists.

~~~~ {.haskell include="src/data.hs"}
~~~~

We can also write generic operations to for instance count the number of parameters in a data type.

```haskell
numHoles :: Data a => a -> Int
numHoles = gmapQl (+) 0 (const 1)

example1 :: Int
example1 = numHoles (1,2,3,4,5,6,7)
-- 7

example2 :: Int
example2 = numHoles (Just 3)
-- 1
```

This method adapts itself well to generic traversals but the types quickly become rather hairy when dealing
anymore more complicated involving folds and unsafe coercions.


Generic
-------

The most modern method of doing generic programming uses type families to achieve a better of deriving the
structural properties of arbitrary type classes.  Generic implements a typeclass with an associated type
``Rep`` ( Representation ) together with a pair of functions that form a 2-sided inverse ( isomorphism ) for
converting to and from the associated type and the derived type in question.

```haskell
class Generic a where
  type Rep a
  from :: a -> Rep a
  to :: Rep a -> a

class Datatype d where
  datatypeName :: t d f a -> String
  moduleName :: t d f a -> String

class Constructor c where
  conName :: t c f a -> String
```

[GHC.Generics](https://www.haskell.org/ghc/docs/7.4.1/html/libraries/ghc-prim-0.2.0.0/GHC-Generics.html)
defines a set of named types for modeling the various structural properties of types in available in Haskell.

```haskell
-- | Sums: encode choice between constructors
infixr 5 :+:
data (:+:) f g p = L1 (f p) | R1 (g p)

-- | Products: encode multiple arguments to constructors
infixr 6 :*:
data (:*:) f g p = f p :*: g p

-- | Tag for M1: datatype
data D
-- | Tag for M1: constructor
data C

-- | Constants, additional parameters and recursion of kind *
newtype K1 i c p = K1 { unK1 :: c }

-- | Meta-information (constructor names, etc.)
newtype M1 i c f p = M1 { unM1 :: f p }

-- | Type synonym for encoding meta-information for datatypes
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors
type C1 = M1 C
```

Using the deriving mechanics GHC can generate this Generic instance for us mechanically, if we were to write
it by hand for a simple type it might look like this:

~~~~ {.haskell include="src/generics.hs"}
~~~~

Use ``kind!`` in GHCi we can look at the type family ``Rep`` associated with a Generic instance. 

```haskell
λ: :kind! Rep Animal
Rep Animal :: * -> *
= M1 D T_Animal (M1 C C_Dog U1 :+: M1 C C_Cat U1)

λ: :kind! Rep ()
Rep () :: * -> *
= M1 D GHC.Generics.D1() (M1 C GHC.Generics.C1_0() U1)

λ: :kind! Rep [()]
Rep [()] :: * -> *
= M1
    D
    GHC.Generics.D1[]
    (M1 C GHC.Generics.C1_0[] U1
     :+: M1
           C
           GHC.Generics.C1_1[]
           (M1 S NoSelector (K1 R ()) :*: M1 S NoSelector (K1 R [()])))
```

Now the clever bit, instead writing our generic function over the datatype we instead write it over the Rep
and then reify the result using ``from``. Some for an equivelant version of Haskell's default ``Eq`` that
instead uses generic deriving we could write:

```haskell
class GEq' f where
  geq' :: f a -> f a -> Bool

instance GEq' U1 where
  geq' _ _ = True

instance (GEq c) => GEq' (K1 i c) where
  geq' (K1 a) (K1 b) = geq a b

instance (GEq' a) => GEq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

-- Equality for sums.
instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

-- Equality for products.
instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2
```

Now to to accommodate the two methods of writing classes (generic-deriving or custom implementions) we can use
``DefaultSignatures`` extension to allow the user to leave typeclass functions blank and defer to the Generic
or to define their own.

```haskell
{-# LANGUAGE DefaultSignatures #-}

class GEq a where 
  geq :: a -> a -> Bool

  default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
  geq x y = geq' (from x) (from y)
```

Now anyone using our library need only derive Generic and create an empty instance of our typeclass instance
without writing any boilerplate for GEq. 

See: 

* [Andres Loh: Datatype-generic Programming in Haskell](http://www.andres-loeh.de/DGP-Intro.pdf)
* [generic-deriving](http://hackage.haskell.org/package/generic-deriving-1.6.3)


Generic Deriving
----------------

GHC.Generics, we can use GHC to do lots of non-trivial code generation which works spectacularly well.

The [hashable](http://hackage.haskell.org/package/hashable) library allows us to derive hashing functions.

~~~~ {.haskell include="src/hashable.hs"}
~~~~

The [cereal](http://hackage.haskell.org/package/cereal-0.4.0.1) library allows us to automatically derive a binary
representation.

~~~~ {.haskell include="src/cereal.hs"}
~~~~

The [aeson](http://hackage.haskell.org/package/aeson) library allows us to derive JSON representations for
JSON instances.

~~~~ {.haskell include="src/derive_aeson.hs"}
~~~~

The [derive](http://hackage.haskell.org/package/derive) library allows us to derive a variety of instances,
include Arbitrary for QuickCheck.

~~~~ {.haskell include="src/derive.hs"}
~~~~

See:

* [A Generic Deriving Mechanism for Haskell](http://dreixel.net/research/pdf/gdmh.pdf)

Data Structures
===============

DList
-----

A dlist is a list-like structure that is optimized for O(1) append operations. It is specifically suited for
operations which are append-only and need only access it when manifesting it as a list, which can be done in
linear time. It is particularly well-suited for use in the Writer monad.

~~~~ {.haskell include="src/dlist.hs"}
~~~~

Sequence
--------

The sequence data structure behaves structurally similar to list but is optimized for append/prepend
operations.

~~~~ {.haskell include="src/sequence.hs"}
~~~~

Unordered-Containers
====================

```haskell
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
```

Both the ``HashMap`` and ``HashSet`` are purely functional data structures that are drop in replacements for
the ``containers`` equivalents but with more efficient space and time performance. Additionally all stored
elements must have a ``Hashable`` instance.

~~~~ {.haskell include="src/unordered.hs"}
~~~~

See: 

* [Johan Tibell: Announcing Unordered Containers](http://blog.johantibell.com/2012/03/announcing-unordered-containers-02.html)

Hashtables
==========

Hashtables provides hashtables with efficient lookup within the ST or IO monad.

~~~~ {.haskell include="src/hashtables.hs"}
~~~~

```haskell
new :: ST s (HashTable s k v)
insert :: (Eq k, Hashable k) => HashTable s k v -> k -> v -> ST s ()
lookup :: (Eq k, Hashable k) => HashTable s k v -> k -> ST s (Maybe v)
```

FFI
===

Pure Functions
--------------

Wrapping pure C functions with primitive types is trivial.

~~~~ {.cpp include="src/simple.c"}
~~~~

~~~~ {.haskell include="src/simple_ffi.hs"}
~~~~

Storable Arrays
----------------

There exists a ``Storable`` typeclass that can be used to provide low-level access to the memory underlying
Haskell values. The Prelude defines Storable interfaces for most of the basic types as well as types in the
``Foreign.C`` library.

```haskell
class Storable a where
  sizeOf :: a -> Int
  alignment :: a -> Int
  peek :: Ptr a -> IO a
  poke :: Ptr a -> a -> IO ()
```

To pass arrays from Haskell to C we can again use Storable Vector and several unsafe operations to grab a
foreign pointer to the underlying data that can be handed off to C. Once we're in C land, nothing will protect
us from doing evil things to memory!

~~~~ {.cpp include="src/qsort.c"}
~~~~

~~~~ {.haskell include="src/ffi.hs"}
~~~~

The names of foreign functions from a C specific header file can qualified.

```haskell
foreign import ccall unsafe "stdlib.h malloc"
    malloc :: CSize -> IO (Ptr a)
```

Prepending the function name with a ``&`` allows us to create a reference to the function itself.

```haskell
foreign import ccall unsafe "stdlib.h &malloc"
    malloc :: FunPtr a
```

Missing Functions
=================

split
-----

The [split](http://hackage.haskell.org/package/split-0.1.1/docs/Data-List-Split.html) package provides a
variety of missing functions for splitting list and string types.

~~~~ {.haskell include="src/split.hs"}
~~~~

monad-loops
-----------

The [monad-loops](http://hackage.haskell.org/package/monad-loops-0.4.2/docs/Control-Monad-Loops.html) package
provides a variety of missing functions for control logic in monadic contexts.


```haskell
whileM :: Monad m => m Bool -> m a -> m [a]
untilM :: Monad m => m a -> m Bool -> m [a]
iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
whileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m [b]
```

Diagrams
========

A parser combinator library for generating vector images to SVG and a variety of other formats.

~~~~ {.haskell include="src/diagrams.hs"}
~~~~

```bash
$ runhaskell diagram1.hs -w 256 -h 256 -o diagram1.svg
```

![](img/diagram1.svg)

Parsec
======

For parsing in Haskell it is quite common to use a family of libraries known as *Parser Combinators* which let
us write code to generate parsers which themselves looks very similar to the parser grammar itself! 

              Combinators   
-----------   ------------
``<|>``       The choice operator tries to parse the first argument before proceeding to the second. Can be chained sequentially to a generate a sequence of options.
``many``      Consumes an arbitrary number of patterns matching the given pattern and returns them as a list.
``many1``     Like many but requires at least one match. 
``optional``  Optionally parses a given pattern returning it's value as a Maybe.
``try``       Backtracking operator will let us parse ambiguous matching expressions and restart with a different pattern.

There are two styles of writing Parsec, one can choose to write with monads or with applicatives.

```haskell
parseM :: Parser Expr
parseM = do
  a <- identifier
  char '+'
  b <- identifier
  return $ Add a b
```

The same code written with applicatives uses the ``(<*)`` to skip over unneeded tokens.

```haskell
parseA :: Parser Expr
parseA = Add <$> identifier <* char '+' <*> identifier
```

For a full example, consider the lambda calculus: 

~~~~ {.haskell include="src/parser.hs"}
~~~~

Trying it out:

```bash
λ: main
1+2
Op Add (Num 1) (Num 2)

\i -> \x -> x
Lam "i" (Lam "x" (Var "x"))

\s -> \f -> \g -> \x -> f x (g x)
Lam "s" (Lam "f" (Lam "g" (Lam "x" (App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))))))
```

Custom Operators
----------------

It's easy to hack together parsers that are internally stateful, for example adding operators that can defined
at parse-time and dynamically added to the ``expressionParser`` table.

~~~~ {.haskell include="src/parsec_operators.hs"}
~~~~

For example input try:

```haskell
infixl 3 ($);
infixr 4 (#);

infix 4 (.);

prefix 10 (-);
postfix 10 (!);

let z = y in a $ a $ (-a)!;
let z = y in a # a # a $ b; let z = y in a # a # a # b;
```

Attoparsec
==========

Attoparsec is a parser combinator like Parsec but more suited for bulk parsing of large text and binary files
instead of parsing language syntax to ASTs. When written properly Attoparsec parsers can be extremely
efficient.

~~~~ {.haskell include="src/attoparsec.hs"}
~~~~

Uniplate
========

Uniplate is a generics library for writing traversals and transformation for arbitrary data structures. It is
extremely useful for writing AST transformations and rewrite systems.

```haskell
plate :: from -> Type from to
(|*)  :: Type (to -> from) to -> to -> Type from to
(|-)  :: Type (item -> from) to -> item -> Type from to

descend   :: Uniplate on => (on -> on) -> on -> on
transform :: Uniplate on => (on -> on) -> on -> on
rewrite   :: Uniplate on => (on -> Maybe on) -> on -> on
```

The ``descend`` function will apply a function to each immediate descendent of an expression and then combines
them up into the parent expression.

The ``transform`` function will perform a single pass bottom-up transformation of all terms in the expression.

The ``rewrite`` function will perform a exhaustive transformation of all terms in the expression to fixed
point, using Maybe to signify termination.

~~~~ {.haskell include="src/uniplate.hs"}
~~~~

Alternatively Uniplate instances can be derived automatically from instances of Data without the need to
explicitly write a Uniplate instance.

```haskell
import Data.Data
import Data.Typeable
import Data.Generics.Uniplate.Data

data Expr a
  = Fls
  | Tru
  | Lit a
  | Not (Expr a)
  | And (Expr a) (Expr a)
  | Or (Expr a) (Expr a)
  deriving (Data, Typeable, Show, Eq)
```

Criterion
=========

Criterion is a statistically aware benchmarking tool.

```haskell
whnf :: (a -> b) -> a -> Pure
nf :: NFData b => (a -> b) -> a -> Pure
bench :: Benchmarkable b => String -> b -> Benchmark
```

~~~~ {.haskell include="src/criterion.hs"}
~~~~

```haskell
$ runhaskell criterion.hs
warming up
estimating clock resolution...
mean is 2.349801 us (320001 iterations)
found 1788 outliers among 319999 samples (0.6%)
  1373 (0.4%) high severe
estimating cost of a clock call...
mean is 65.52118 ns (23 iterations)
found 1 outliers among 23 samples (4.3%)
  1 (4.3%) high severe

benchmarking naive/fib 10
mean: 9.903067 us, lb 9.885143 us, ub 9.924404 us, ci 0.950
std dev: 100.4508 ns, lb 85.04638 ns, ub 123.1707 ns, ci 0.950

benchmarking naive/fib 20
mean: 120.7269 us, lb 120.5470 us, ub 120.9459 us, ci 0.950
std dev: 1.014556 us, lb 858.6037 ns, ub 1.296920 us, ci 0.950

benchmarking de moivre/fib 10
mean: 7.699219 us, lb 7.671107 us, ub 7.802116 us, ci 0.950
std dev: 247.3021 ns, lb 61.66586 ns, ub 572.1260 ns, ci 0.950
found 4 outliers among 100 samples (4.0%)
  2 (2.0%) high mild
  2 (2.0%) high severe
variance introduced by outliers: 27.726%
variance is moderately inflated by outliers

benchmarking de moivre/fib 20
mean: 8.082639 us, lb 8.018560 us, ub 8.350159 us, ci 0.950
std dev: 595.2161 ns, lb 77.46251 ns, ub 1.408784 us, ci 0.950
found 8 outliers among 100 samples (8.0%)
  4 (4.0%) high mild
  4 (4.0%) high severe
variance introduced by outliers: 67.628%
variance is severely inflated by outliers
```

Optparse-Applicative
====================

Optparse applicative is a library for parsing command line options with a inferface similar to parsec that
makes also makes heavy use of monoids to combine operations.

~~~~ {.haskell include="src/optparse_applicative.hs"}
~~~~

See:

* [optparse-applicative](https://github.com/pcapriotti/optparse-applicative)

Haskeline
=========

Haskeline is cross-platform readline support which plays nice with GHCi as well.

```haskell
runInputT :: Settings IO -> InputT IO a -> IO a
getInputLine :: String -> InputT IO (Maybe String)
```

~~~~ {.haskell include="src/haskelline.hs"}
~~~~

Conduits / Pipes
================

Pipes
-----

```haskell
await :: Monad m => Proxy () a y' y m a
yield :: Monad m => a -> Proxy x' x () a m ()

runEffect :: Monad m => Effect m r -> m 
toListM :: Monad m => Producer a m () -> m [a]
```

Pipes is a stream processing library with a strong emphasis on the static semantics of composition. The
simplest usage is to connect "pipe" functions with a ``(>->)`` composition operator, where each component can
``await`` and ``yield`` to push and pull values along the stream. Most notably pipes can be used to manage the
life cycle of lazy IO resources and can safely handle failures and resource termination gracefully.

~~~~ {.haskell include="src/pipes.hs"}
~~~~

For example we could construct a "FizzBuzz" pipe.


~~~~ {.haskell include="src/pipes_io.hs"}
~~~~

See: 

* [Pipes Tutorial](http://hackage.haskell.org/package/pipes-4.1.0/docs/Pipes-Tutorial.html)

Conduits
--------

```haskell
await :: Monad m => ConduitM i o m (Maybe i)
yield :: Monad m => o -> ConduitM i o m ()
($$) :: Monad m => Source m a -> Sink a m b -> m b
(=$) :: Monad m => Conduit a m b -> Sink b m c -> Sink a m c

type Sink i = ConduitM i Void
type Source m o = ConduitM () o m ()
type Conduit i m o = ConduitM i o m ()
```

Conduits are conceptually similar though philosophically different approach to the same problem of constant
space deterministic resource handling for IO resources.

The first initial differnce is that await function now returns a ``Maybe`` which allows different handling of
termination. The composition operators are also split into a connecting operator (``$$``) and a fusing
operator (``=$``) for combining Sources and Sink and a Conduit and a Sink respectively.

~~~~ {.haskell include="src/conduit.hs"}
~~~~

See:

* [Conduit Overview](https://www.fpcomplete.com/user/snoyberg/library-documentation/conduit-overview)

Aeson
=====

Aeson is library for efficient parsing and generating JSON.

```haskell
decode :: FromJSON a => ByteString -> Maybe a
encode :: ToJSON a => a -> ByteString
eitherDecode :: FromJSON a => ByteString -> Either String a

fromJSON :: FromJSON a => Value -> Result a
toJSON :: ToJSON a => a -> Value
```

We'll work with this contrived example:

~~~~ {.json include="src/example.json"}
~~~~

Aeson uses several high performance data structures (Vector, Text, HashMap) by default instead of the naive
versions so typically using Aeson will require that us import them and use ``OverloadedStrings`` when
indexing into objects.

```haskell
type Object = HashMap Text Value

type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
```

Unstructured
------------

In dynamic scripting languages it's common to parse amorphous blobs of JSON without any a priori structure and
then handle validation problems by throwing exceptions while traversing it. We can do the same using Aeson and
the Maybe monad.

~~~~ {.haskell include="src/aeson_unstructured.hs"}
~~~~

Structured
----------

This isn't ideal since we've just smeared all the validation logic across our traversal logic instead of
separating concerns and handling validation in separate logic. We'd like to describe the structure before-hand
and the invalid case separately. Using Generic also allows Haskell to automatically write the serializer and
deserializer between our datatype and the JSON string based on the names of record field names.

~~~~ {.haskell include="src/aeson_structured.hs"}
~~~~

Now we get our validated JSON wrapped up into a nicely typed Haskell ADT.

```haskell
Data
  { id = 1
  , name = "A green door"
  , price = 12
  , tags = [ "home" , "green" ]
  , refs = Refs { a = "red" , b = "blue" }
  }
```

The functions ``fromJSON`` and ``toJSON`` can be used to convert between this sum type and regular Haskell
types with.

```haskell
data Result a = Error String | Success a
```

```haskell
λ: fromJSON (Bool True) :: Result Bool
Success True

λ: fromJSON (Bool True) :: Result Double
Error "when expecting a Double, encountered Boolean instead"
```

Cassava
=======

Cassava is an efficient CSV parser library. We'll work with this tiny snippet from the iris dataset:

~~~~ {.perl include="src/iris.csv"}
~~~~

Unstructured
------------

Just like with Aeson if we really want to work with unstructured data the library accommodates this.

~~~~ {.haskell include="src/cassava_unstructured.hs"}
~~~~

We see we get the nested set of stringy vectors:


```haskell
[ [ "sepal_length"
  , "sepal_width"
  , "petal_length"
  , "petal_width"
  , "plant_class"
  ]
, [ "5.1" , "3.5" , "1.4" , "0.2" , "Iris-setosa" ]
, [ "5.0" , "2.0" , "3.5" , "1.0" , "Iris-versicolor" ]
, [ "6.3" , "3.3" , "6.0" , "2.5" , "Iris-virginica" ]
]
```

Structured
----------

Just like with Aeson we can use Generic to automatically write the deserializer between our CSV data and our
custom datatype.

~~~~ {.haskell include="src/cassava_structured.hs"}
~~~~

And again we get a nice typed ADT as a result.

```haskell
[ Plant
    { sepal_length = 5.1
    , sepal_width = 3.5
    , petal_length = 1.4
    , petal_width = 0.2
    , plant_class = "Iris-setosa"
    }
, Plant
    { sepal_length = 5.0
    , sepal_width = 2.0
    , petal_length = 3.5
    , petal_width = 1.0
    , plant_class = "Iris-versicolor"
    }
, Plant
    { sepal_length = 6.3
    , sepal_width = 3.3
    , petal_length = 6.0
    , petal_width = 2.5
    , plant_class = "Iris-virginica"
    }
]
```

Warp
====

Warp is a web server, it writes data to sockets quickly.

~~~~ {.haskell include="src/warp.hs"}
~~~~

See: 

* [Warp](http://aosabook.org/en/posa/warp.html)

Scotty
======

Continuing with our trek through web libraries, Scotty is a web microframework similar in principle to Flask
in Python or Sinatra in Ruby.

~~~~ {.haskell include="src/scotty.hs"}
~~~~

Of importance to note is the Blaze library used here overloads do-notation is not itself a monad.

See:

* [Making a Website with Haskell](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html)

Acid State
==========

Acid-state allows us to build a "database on demand" for arbitrary Haskell datatypes that guarantees atomic
transactions. For example, we can build a simple key-value store wrapped around the Map type.

~~~~ {.haskell include="src/acid.hs"}
~~~~

Safe Haskell
============

The Safe Haskell language extensions allow us to restrict the use of unsafe language features using
``-XSafe``, or at least on our honor claim that we've proved that our code is referentially transparent using
``-XTrustworthy``.

```haskell
{-# LANGUAGE Safe #-}
{-# LANGUAGE Trustworthy #-}
```

~~~~ {.haskell include="src/safe.hs"}
~~~~

```haskell
Unsafe.Coerce: Can't be safely imported!
The module itself isn't safe.
```

GHC Core
========

To inspect the core from GHCi we can invoke it using the following flags and the alias:

```bash
alias ghci-core="ghci -ddump-simpl -dsuppress-idinfo \
-dsuppress-coercions -dsuppress-type-applications \ 
-dsuppress-uniques -dsuppress-module-prefixes"
```

At the interactive prompt we can then explore the core representation interactively:

```bash
$ ghci-core
λ: let f x = x + 2 ; f :: Int -> Int

==================== Simplified expression ====================
returnIO
  (: ((\ (x :: Int) -> + $fNumInt x (I# 2)) `cast` ...) ([]))

λ: let f x = (x, x)

==================== Simplified expression ====================
returnIO (: ((\ (@ t) (x :: t) -> (x, x)) `cast` ...) ([]))
```


[ghc-core](http://hackage.haskell.org/package/ghc-core) is also very useful for looking at GHC's compilation
artifacts.

```bash
$ ghc-core --no-cast --no-asm
```

Core from GHC is roughly human readable, but it's helpful to look at simple human written examples to get the
hang of what's going on. Of important note is that the Λ and λ for type-level and value-level lambda
abstraction are represented by the same symbol (``\``) in core, which is a simplifying detail of the GHC's
implementation but a source of some confusion when starting.

```haskell
id :: a -> a
id x = x
```

```haskell
id :: forall a. a -> a
id = \ (@ a) (x :: a) -> x
```

```haskell
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
```

```haskell
compose :: forall b c a. (b -> c) -> (a -> b) -> a -> c
compose = \ (@ b) (@ c) (@ a) (f1 :: b -> c) (g :: a -> b) (x1 :: a) -> f1 (g x1)
```

```haskell
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
```

```haskell
map :: forall a b. (a -> b) -> [a] -> [b]
map =
  \ (@ a) (@ b) (f :: a -> b) (xs :: [a]) ->
    case xs of _ {
      []     -> GHC.Types.[] @ b;
      : y ys -> GHC.Types.: @ b (f y) (map @ a @ b f ys)
    }
```

```haskell
x `seq` y
```

```haskell
case x of _ { 
  __DEFAULT -> y 
}
```

Unboxed Values
--------------

The usual integer type in Haskell can be considered to be a regular algebraic datatype with a special
constructor.

```haskell
λ: :set -XMagicHash
λ: :m +GHC.Types
λ: :m +GHC.Prim
λ: :i Int
data Int = I# Int#      -- Defined in GHC.Types
```

The function for integer arithmetic used in the ``Num`` typeclass for ``Int`` is just pattern matching on this
type to reveal the underlying unboxed value, performing the builtin arithmetic and then performing the packing
up into ``Int`` again.

```haskell

(+#) :: Int# -> Int# -> Int#
(I# x) `plusInt`  (I# y) = I# (x +# y)
```

Where ``(+#)`` is a low level function built into GHC that maps to unboxed integer arithmetic directly.

```haskell
plusInt a b = case a of {
    (I# a_) -> case b of {
      (I# b_) -> I# (+# a_ b_);
    };
};
```

Since the Int type we'd write down for normal logic is itself boxed, we'd sometimes like to inform GHC that
our value should is just a fixed unboxed value on the heap and to refer to it by value instead of by
reference. In C the rewrite would be like the following:

```cpp
struct A {
  int *a;
};

struct A {
  int a;
};
```

Effectively we'd like to be able to define our constructor to be stored as:

```haskell
data A = A #Int
```
But maintain all our logic around as if it were written against Int, performing the boxing and unboxing where
needed.

```haskell
data A = A !Int
```

To do this there is the ``UNPACK`` pragma or ``-funbox-strict-fields`` to inform GHC to perform the rewrite we
want.

```haskell
data A = A {-# UNPACK #-} !Int
```

See: 

* [Unboxed Values as First-Class Citizens](http://www.haskell.org/ghc/docs/papers/unboxed-values.ps.gz)

LLVM General
============

llvm-general is so useful I've written an entire tutorial on it.

See: 

* [Implementing a JIT Compiled Language with Haskell and LLVM](http://www.stephendiehl.com/llvm/)

van Laarhoven Lenses
======

There are two implementations of note that are mostly compatible but differ in scope:

* *lens* - The kitchen sink library with a wide variety of instances for many common libraries.
* *lens-family-core* - The core abstractions in a standalone library with minimal dependencies.

lens
----

At it's core a lens is a type of the form:

```haskell
type Lens a b = forall f. Functor f => (b -> f b) -> (a -> f a)
```

Using this type and some related machinery we get a framework for building a very general set of combinators
for working with datatypes of arbitrary structure and targets within their substucture. Some of the
combinators are:

Combinator      Description
-------------   -----------------------------
``set``         Replace target with a value and return updated structure.
``over``        Update targets with a function and return updated structure.
``view``        View a single target or fold the targets of a monoidal quantity.
``to``          Construct a retrieval function from an arbitrary Haskell function.
``traverse``    Map each element of a structure to an action and collect results.
``ix``          Target the given index of a generic indexable structure.
``toListOf``    Return a list of the targets.

Constructing the lens field types from an arbitrary datatype involves a bit of boilerplate code generation,
but template Haskell can take care of this construction for us at compile time. See derivation in links for
more details. 

The simplest usage of lens is simply as a more compositional way of dealing with record access and updates. 

~~~~ {.haskell include="src/simplelens.hs"}
~~~~

Of course this is just the surface, the real strength comes when dealing with complex and deeply nested
structures:

~~~~ {.haskell include="src/lens.hs"}
~~~~

Lens also provides us with an optional dense slurry of operators that expand into combinations of the core
combinators. Many of the operators do have a [consistent naming
scheme](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial#actually-there-are-a-whole-lot-of-operators-in-lens---over-100).

Of course lenses generalize to arbitrary data structures and computations, not just nested records:

~~~~ {.haskell include="src/complexlens.hs"}
~~~~

See: 

* [A Little Lens Tutorial](https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
* [CPS based functional references](http://twanvl.nl/blog/haskell/cps-functional-references)
* [Lens Derivation](https://github.com/ekmett/lens/wiki/Derivation)
* [Lens infix operators](https://github.com/quchen/articles/blob/master/lens-infix-operators.md)

State and Zooms
---------------

Within the context of the state monad there are a particularly useful set of lens patterns.

* ``use`` - View a target from the state of the State monad.
* ``assign`` - Replace the target within a State monad.
* ``zoom`` - Modify a target of the state with a function and perform it on the global state of the State monad.

So for example if we wanted to write a little physics simulation of the random motion of particles in a box.
We can use the ``zoom`` function to modify the state of our particles in each step of the simulation.

~~~~ {.haskell include="src/zoom.hs"}
~~~~

This results in a final state like the following.

```haskell
Box
  { _particles =
      [ Particle
          { _pos = Vector { _x = 3.268546939011934 , _y = 4.356638656040016 }
          , _vel =
              Vector { _x = 0.6537093878023869 , _y = 0.8713277312080032 }
          }
      , Particle
          { _pos =
              Vector { _x = 0.5492296641559635 , _y = 0.27244422070641594 }
          , _vel =
              Vector { _x = 0.1098459328311927 , _y = 5.448884414128319e-2 }
          }
      , Particle
          { _pos =
              Vector { _x = 3.961168796078436 , _y = 4.9317543172941765 }
          , _vel =
              Vector { _x = 0.7922337592156872 , _y = 0.9863508634588353 }
          }
      , Particle
          { _pos =
              Vector { _x = 4.821390854065674 , _y = 1.6601909953629823 }
          , _vel =
              Vector { _x = 0.9642781708131349 , _y = 0.33203819907259646 }
          }
      , Particle
          { _pos =
              Vector { _x = 2.6468253761062943 , _y = 2.161403445396069 }
          , _vel =
              Vector { _x = 0.5293650752212589 , _y = 0.4322806890792138 }
          }
      ]
  }
```

Lens + Aeson
------------

One of the best showcases for lens is writing transformations over arbitrary JSON structures. For example
consider some sample data related to Kiva loans.

~~~~ {.json include="src/kiva.json"}
~~~~

Then using ``Data.Aeson.Lens`` we can traverse the structure using our lens combinators.

~~~~ {.haskell include="src/lens_aeson.hs"}
~~~~

```haskell
[13.75,93.75,43.75,63.75,93.75,93.75,93.75,93.75]
```

lens-family
-----------

The interface for ``lens-family`` is very similar to ``lens`` but with a smaller API and core.

~~~~ {.haskell include="src/lens_family.hs"}
~~~~

Categories
==========

Categories
----------

The most basic structure is a category which is an algebraic structure of objects (``Obj``) and morphisms
(``Hom``) with the structure that morphisms compose associatively and the existence of a identity morphism for
each object.

With kind polymorphism enabled we can write down the general category parameterized bya type variable "c" for
category, and the instance ``Hask`` the category of Haskell types with functions between types as morphisms.

~~~~ {.haskell include="src/categories.hs"}
~~~~

Isomorphisms
------------

Two objects of a category are said to be isomorphic if there exists a morphism with 2-sided inverse.

```haskell
f  :: a -> b
f' :: b -> a
```

Such that:

```haskell
f . f' = id
f'. f  = id
```

```haskell
data Iso a b = Iso { to :: a -> b, from :: b -> a }

instance Category Iso where
  id = Iso id id
  (Iso f f') . (Iso g g') = Iso (f . g) (g' . f')
```

Duality
-------

One of the central ideas is the notion of duality, that reversing some internal structure yields a new
structure with a "mirror" set of theorems. The dual of a category reverse the direction of the morphisms
forming the category C<sup>Op</sup>.

~~~~ {.haskell include="src/dual.hs"}
~~~~

See:

* [Duality for Haskellers](http://blog.ezyang.com/2012/10/duality-for-haskellers/)

Functors
--------

Functors are mappings between the objects and morphisms of categories that preserve identities and
composition.

~~~~ {.haskell include="src/functors.hs"}
~~~~

```haskell
fmap id ≡ id
fmap (a . b) ≡ (fmap a) . (fmap b)
```

Natural Transformations
-----------------------

Natural transformations are mappings between functors that are invariant under interchange of morphism
composition order.

```haskell
type Nat f g = forall a. f a -> g a
```

Such that for a natural transformation ``h`` we have:

```haskell
fmap f . h ≡ h . fmap f 
```

The simplest example is between (f = List) and (g = Maybe) types.

```haskell
headMay :: forall a. [a] -> Maybe a
headMay []     = Nothing
headMay (x:xs) = Just x
```

Regardless of how we chase ``safeHead``, we end up with the same result.

```haskell
fmap f (headMay xs) ≡ headMay (fmap f xs)
```

```haskell
fmap f (headMay [])
= fmap f Nothing
= Nothing

headMay (fmap f [])
= headMay []
= Nothing
```

```haskell
fmap f (headMay (x:xs))
= fmap f (Just x)
= Just (f x)

headMay (fmap f (x:xs))
= headMay [f x]
= Just (f x)
```

Yoneda Lemma
------------

The Yoneda lemma is an elementary, but deep result in Category theory. The Yoneda lemma states that for any
functor ``F``, the types ``F a`` and ``∀ b. (a -> b) -> F b`` are isomorphic.

```haskell
{-# LANGUAGE RankNTypes #-}

embed :: Functor f => f a -> (forall b . (a -> b) -> f b)
embed x f = fmap f x

unembed :: Functor f => (forall b . (a -> b) -> f b) -> f a
unembed f = f id
```

So that we have:

```haskell
embed . unembed ≡ id
unembed . embed ≡ id
```

The most broad hand-wavy statement of the theorem is that an object in a category can be represented by the
set of morphisms into it, and that the information about these morphisms alone sufficiently determines all
properties of the object itself.

In terms of Haskell types, given a fixed type ``a`` and a functor ``f``, if we have some a higher order
polymorphic function ``g`` that when given a function of type ``a -> b`` yields ``f b`` then the behavior
``g`` is entirely determined by ``a -> b`` and the behavior of ``g`` can written purely in terms of ``f a``.

See:

* [Reverse Engineering Machines with the Yoneda Lemma](http://blog.sigfpe.com/2006/11/yoneda-lemma.html)

Kleisli Category
----------------

Kleisli composition (i.e. Kleisli Fish) is defined to be:

```haskell
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g ≡ \x -> f x >>= g 
```

The monad laws stated in terms of the Kleisli category of a monad ``m`` are stated much more symmetrically as
one associativiy law and two identity laws.

```haskell
(f >=> g) >=> h ≡ f >=> (g >=> h)
return >=> f ≡ f
f >=> return ≡  f
```

Stated simply that the monad laws above are just the category laws in the Kleisli category.

~~~~ {.haskell include="src/kleisli.hs"}
~~~~

For example, ``Just`` is just an identity morphism in the Kleisli category of the ``Maybe`` monad.

```haskell
Just >=> f ≡ f
f >=> Just ≡ f
```

Mathematics
-----------

Just as in Haskell we try to unify the common ideas from distinct structures, we can ask a simple question
like what the fundamental notion of a group is for different mathematical categories:


Category   Description                             Group
--------   ----------------------------------      ----------
**Set**    The category of sets with objects as    Abelian group
           sets and morphisms are functions 
           between them.
**Man**    The category of manifolds with          Lie group
           objects as manifolds and morphisms 
           as differentiable functions between
           manifolds.
**Top**    The category of topological spaces      Topological group
           with objects as topological spaces 
           as and continuous functions between
           spaces.
**Grp**    The category of Abelian groups,         Category objects
           with groups as objects and group 
           homomorphism between groups.

Some deep results in algebraic topology about the homology groups of topological spaces turn out stated very
concisely as the relationships between functors and natural isomorphisms of these four categories!

Resources
---------

* [Category Theory, Awodey](http://www.amazon.com/Category-Theory-Oxford-Logic-Guides/dp/0199237182)
* [Category Theory Foundations](https://www.youtube.com/watch?v=ZKmodCApZwk)
* [The Catsters](http://www.youtube.com/user/TheCatsters)
