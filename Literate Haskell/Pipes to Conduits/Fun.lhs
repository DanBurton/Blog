A Functor in Haskell is a type of kind `f :: * -> *`,
which supports the operation `fmap :: (a -> b) -> f a -> f b`.
Many "container" types are Functors,
including the List type.
But we're not going to talk about "containers", per se.
We're going to explore a few of the simplest functors
that we can squeeze out of the Haskell type system.
Of course, I don't know the actual name of some of these,
so you'll have to forgive me for giving them pet names instead.

Our end goal in exporing these functors is
to reproduce the Conduit library
by assembling pieces of it,
one functor at a time.
For this post, we're just going to survey
various functors, and ways to compose them.
I'll also touch lightly on how they play with
the Free Monad Transformer,
though serious discussion of such will be left
to later posts.

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module Fun where


The Identity functor
--------------------------------------------------

> newtype Identity next = Identity next

The Identity functor trivially wraps a value.
In order to implement fmap, it just applies the function
directly to the value inside.

> instance Functor Identity where
>   fmap f (Identity next) = Identity (f next)

When used with the Free Monad Transformer,
the Identity monad trivially grants you the ability
to represent "the thing that comes next".
This will be convenient for us sometime around
part 5 of this series.


The Empty functor
--------------------------------------------------

> data Empty next = Empty

The Empty functor contains no information.
It admits the type variable, but is otherwise
nothing but the trivial value, `()`.

> instance Functor Empty where
>   fmap _f Empty = Empty

When used with the Free Monad Transformer,
the Empty functor allows you to short-circuit computation.
The Free Monad Transformer works by stacking functors
up, but as you can see, the Empty functor
has no room for other functors to live inside of it.


The Const functor
--------------------------------------------------

> newtype Const a next = Const a

The Const functor is very similar to
the Empty functor, except that it contains
some actual value, which remains untouched
by functor operations.

> instance Functor (Const a) where
>   fmap _f (Const a) = Const a

When used with the Free Monad Transformer,
the Const functor allows you to
terminate computation while providing some information.
Joining this functor with the Identity functor
will allow us to supply information *without* terminating
computation (because the Identity functor gives a space
for the "next" computation), which will be the
heart of our `yield` functionality.


The (a ->) functor
--------------------------------------------------

> newtype Fun a next = Fun (a -> next)

Functions, as you may know, are functors.
In order to fmap onto a function, simply wait
until the function has acquired input and produced an output,
and then map onto the function's output.

> instance Functor (Fun a) where
>   fmap f (Fun g) = Fun (\x -> f (g x))

When used with the Free Monad Transformer,
this allows us to represent inversion of control,
or acquiring information from some outside source,
in order to determine what to do next.
This will be the heart of our `await` functionality.

Composing functors
--------------------------------------------------

> newtype (f :.: g) x = Composed (f (g x))

Functors can be composed, and the result is a functor.

> instance (Functor f, Functor g) => Functor (f :.: g) where
>    fmap f (Composed x) = Composed $ fmap (fmap f) x

I won't be using this particular form of functor composition
for future posts, but it was worth noting. Instead,
let's take a look at two other ways to combine functors:

Combining functors (tagged union)
--------------------------------------------------

> infixl 5 :|:
> data (f :|: g) x = L (f x) | R (g x)

If I have two functors `f` and `g`, then their tagged union
is also a functor. We can just tag the `f x` values with `L`
and the `g x` values with `R` so that whenever we come across
some data, we know which of the two functors it actually was.

By case analysis, we can make a tagged union of functors
also be a functor:

> instance (Functor f, Functor g) => Functor (f :|: g) where
>   fmap f (L x) = L (fmap f x)
>   fmap f (R x) = R (fmap f x)

This will be very useful. While in normal code you would just use
Haskell's plain old tagged unions to define a data type:

    data List a = Nil | Cons a (List a)

we're not going to do that, because it's more fun to
take advantage of functory goodness.

You could define a left-only or right-only functor for
a tagged union if you wanted to.


Combining functors (product)
--------------------------------------------------

> infixl 7 :&:
> data (f :&: g) x = f x :&: g x

If I have two functors `f` and `g`, then their product
is also a functor: just perform the fmap on them both
simultaneously.

> instance (Functor f, Functor g) => Functor (f :&: g) where
>   fmap f (l :&: r) = fmap f l :&: fmap f r

Similar to how in Haskell you can provide
multiple pieces of data to a constructor,
we can use `:&:` to bundle information together.

    [haskell]
    type Cons a = Const a :&: Identity
    type Nil = Empty
    type ListF a = Nil :|: Cons a
    type List a = FreeT (ListF a)

Again, left-only or right-only Functor instances are possible,
but unnecessary for my needs.


Next time
--------------------------------------------------

Next time, we'll start by creating an implementation
of Control.Pipe from the pipes package.
Our Pipe type will be able to `yield` and `await`.

Exercise to the reader: try it yourself
before you read the next post!
Here's a little bit to get you started.

    [haskell]
    {-# LANGUAGE TypeOperators #-}
    module Pipe where

    -- "cabal update && cabal install pipes" for this Free module
    import Control.Monad.Trans.Free
    import Fun

    newtype Then    next = Then next         -- Identity
    newtype Yield o next = Yield o           -- Const
    newtype Await i next = Await (i -> next) -- Fun

    type PipeF i o = (??? :&: ???) :|: ???
    type Pipe i o m r = ??? PipeF i o ???

    yield :: o -> Pipe i o m ()
    yield o = ???

    await :: Pipe i o m i
    await = ???

To play with this code, download
[Fun.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/Fun.lhs).

