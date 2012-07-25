Last time we quickly reviewed several
basic Functors in Haskell, and various ways
to combine them. Today, we will
put these functors to good use,
and rewrite Control.Pipe
(not that it needs rewriting;
we're just doing this for fun).

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module Pipe where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> import Fun ((:&:)(..), (:|:)(..))
> 
> import Data.Void (Void)
> import Control.Monad (forever, when)
> import Control.Monad.Trans.Class (lift)


Functors
--------------------------------------------------

I'm going to give new names to three old friends.
These new names will be more convenient and helpful
when dealing with pipe-related concepts.

I'll use a few conventions throughout this code:

 * A pipe's input is referred to by the type variable `i`
 * A pipe's output is referred to by the type variable `o`
 * Monads, as usual, are referred to by the type variable `m`
 * A pipe's return type is referred to as `r`
 * The final type variable of a functor will usually be called `next`

> newtype Then next = Then next            -- Identity
> newtype Yield o next = Yield o           -- Const
> newtype Await i next = Await (i -> next) -- Fun

`Then` embodies knowledge of what to do `next`, while `Await`
represents the need of an `i` in order to determine what's `next`.
`Yield` provides an `o`, which is presumably the `i` that someone else
is awaiting.

The Functor instances are the same as in the last post.

> instance Functor Then where
>   fmap f (Then next) = Then (f next)
>
> instance Functor (Yield o) where
>   fmap _f (Yield o) = Yield o
> 
> instance Functor (Await i) where
>   fmap f (Await g) = Await (f . g)


The Pipe type
--------------------------------------------------

For our pipe primitive `yield`, we want to be able
to continue computation afterwards, so we will
bundle `Yield o` with `Then` to accomplish this.

> type YieldThen o = Yield o :&: Then

At its heart, a Pipe can either Yield(Then) or Await.
We can encode this directly with the `:|:` functor combiner.

> type PipeF i o = YieldThen o :|: Await i

Now we have assembled our functor,
let's create a Free Monad out of it.

> type Pipe i o = FreeT (PipeF i o)

This type is intended to work just like
`Pipe` from Control.Pipe. It has four type parameters:
`Pipe i o m r` (the final two are implied by the partial application
of FreeT). We'll also provide the same convenience synonyms
as Control.Pipe (again, with implied type parameters `m` and `r`):

> type Producer o = Pipe () o
> type Consumer i = Pipe i  Void
> type Pipeline   = Pipe () Void


Working with PipeF
--------------------------------------------------

Unfortunately, `FreeT` introduces some extra layers
of cruft that we have to work through. Our functor-based
approach using `(:|:)` and `(:&:)` introduces even more cruft.
Fear not, it is all very straightforward, and it is an
entirely mechanical process to deal with the cruft.

First, let's define some lifting functions and smart constructors
to help us put the right puzzle pieces in the right places:

> liftYield :: YieldThen o next -> PipeF i o next
> liftYield = L
> 
> liftAwait :: Await i next ->     PipeF i o next
> liftAwait = R
> 
> yieldF :: o -> next ->           PipeF i o next
> yieldF o next = liftYield $ Yield o :&: Then next
> 
> awaitF :: (i -> next) ->         PipeF i o next
> awaitF f = liftAwait $ Await f

Now, to cut down on pattern-matching cruft,
we'll make a case assessment function.

First, consider the FreeT type:

    [haskell]
    newtype FreeT f m r = FreeT
      { runFreeT :: m (FreeF f r (FreeT f m r)) }

Ugh! It looks daunting, but it's really quite straightforward.
First, it is wrapped in a monad, `m next`. Second, it is wrapped in a
`FreeF f r next`, which can be either `Return r` or `Wrap (f next)`.
Finally, `next` is another `FreeT f m r` all over again.

Because we will, to some extent, be mimicking Control.Pipe code,
we will be performing case analysis at the level of FreeF.

> pipeCase :: FreeF (PipeF i o) r next
>          -> (r           -> a) -- Return
>          -> (o -> next   -> a) -- Yield
>          -> ((i -> next) -> a) -- Await
>                          -> a
> pipeCase (Return r) k _ _ = k r
> pipeCase (Wrap (L (Yield o :&: Then next)))
>                     _ k _ = k o next
> pipeCase (Wrap (R (Await f)))
>                     _ _ k = k f


Pipe primitives
--------------------------------------------------

We already created smart constructors `awaitF`
and `yieldF`, which take the appropriate arguments,
and plug them into the correct slots to create a `PipeF`.

By making use of these and `liftF`,
writing the pipe primitives `await` and `yield` is trivial.

> await :: Monad m => Pipe i o m i
> await   = liftF $ awaitF id

> yield :: Monad m => o -> Pipe i o m ()
> yield b = liftF $ yieldF b ()

The trick to using `liftF` is you simply provide
whatever argument to the constructor that makes sense.
For `await`, we need to provide a function `i -> next`,
such that `next` is the result type `i`.
The obvious function `i -> i` is `id`.
For `yield`, we need to come up with something
such that `next` is the result type `()`,
so `()` is what we plug in. `liftF` won't always
suit our needs, but once you grasp the little intuition,
it is quite elegant for the places where it does work.


Pipe composition
--------------------------------------------------

The fundamental thing that you *do* with pipes is
you connect them. By rearranging the type variables,
you can make a `Category`, but I won't bother doing that here.

Pipe composition is driven by the *downstream* pipe.
The arrows point downstream, so `p1 <+< p2` means that p1
is *downstream* of p2, and p2 is *upstream* of p1.

> (<+<) :: Monad m => Pipe i' o m r -> Pipe i i' m r -> Pipe i o m r
> p1 <+< p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1

We begin by running the downstream monadic action
(recall that FreeT is just a newtype around `m (FreeF ...)`)
and performing case analysis on the resultant FreeF.

>   {- Return -} (\r      -> return r)
>   {- Yield  -} (\o next -> wrap $ yieldF o (next <+< p2))

If the downstream pipe is a Return, then we discard the upstream pipe
and return the result. (The expressions on the right-hand side of each
-> are `Pipe`s, so when we say `return r`, we are saying "create the pipe
that trivially returns r".)

If the downstream pipe is yielding, then we create a yield action,
and suspend the composition of whatever comes next after the yield
with the upstream pipe. Remember, `yield` transfers control *downstream*,
so if the downstream of two composed pipes is yielding, then they are *both*
giving up control to a pipe farther *down* the line.
Also recall that `wrap` will take a `PipeF` and make a `Pipe` out of it,
and that `yieldF` makes a `PipeF`.

>   {- Await  -} (\f1     -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2

If the downstream pipe is `Await`ing, then control transfers upstream.
We perform the same trick of extracting and casing on the upstream pipe.

>     {- Return -} (\r      -> return r)
>     {- Yield  -} (\o next -> f1 o <+< next)
>     {- Await  -} (\f2     -> wrap $ awaitF (\i -> p1' <+< f2 i)))

If the upstream pipe is a Return, then we *discard the downstream pipe*
and return the result. That's right: whichever pipe returns first wins,
and shuts down anyone that is composed with it as soon as they give it control.

If the upstream pipe is yielding, then great! We're in the branch where
we happen to know that downstream is `Await`ing, so just pass the yielded
information along, and compose the new downstream pipe with the "next"
part of the upstream one.

If the upstream pipe is awaiting, well, then both upstream *and* downstream
are awaiting, so we transfer control *further* upstream by combining the
two into a single await construct, deferring their composition
until an up-upstream value is available.

And that's it! That wasn't so hard, was it?

> (>+>) :: Monad m => Pipe i i' m r -> Pipe i' o m r -> Pipe i o m r
> (>+>) = flip (<+<)

> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

Running a pipeline is very straightforward.
We simply provide an endless supply of `()` fuel,
and keep cranking the pipeline until it returns something.
We ignore anything it yields; its type is constrained to Void
so it shouldn't be yielding anything in the first place.

> runPipe :: Monad m => Pipeline m r -> m r
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Return -} (\r       -> return r)
>   {- Yield  -} (\_o next -> runPipe next)
>   {- Await  -} (\f       -> runPipe $ f ())

Note that `runPipe` and `<+<` could be considered
two different "interpreters" for pipes.
The Free monad just gives us a convenient way
to assemble the puzzle pieces,
but it is up to us to give the final result meaning
by interpreting it. Conduit's "connect and resume" operator
could be considered yet another "interpreter".

Some basic pipes
-------------------------------------------------

Here's just a few pipes to play with.
Fire up ghci and make sure they work as expected.

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

Pay attention to how the following are implemented,
because next time we're going to have to change them.

> pipe :: Monad m => (i -> o) -> Pipe i o m r
> pipe f = forever $ await >>= yield . f
> 
> idP :: Monad m => Pipe i i m r
> idP = pipe id
> 
> filterP :: Monad m => (i -> Bool) -> Pipe i i m r
> filterP test = forever $ await >>= \x -> when (test x) (yield x)
> 
> printer :: Show i => Consumer i IO r
> printer = forever $ await >>= lift . print

Testing...

    [ghci]
    runPipe $ printer <+< pipe (+1) <+< filterP even <+< fromList [1 .. 5]
    runPipe $ idP <+< idP <+< return "Hello, pipes" <+< idP <+< idP
    runPipe $ return "Downstream drives" <+< return "Upstream doesn't"
    runPipe $ (printer >> return "not hijacked") <+< return "hijacked"

Next time
-------------------------------------------------

Await and yield are great, but the greedy return shutdown behavior
is somewhat disturbing. Next time, we'll tweak the Pipe type,
giving it an "upstream result" type parameter. With that,
result types will be composed, too, and that way
an upstream pipe won't be able to hijack a downstream pipe!

    [haskell]
    type PipeF i o u = ???
    type Pipe i o u = FreeT (PipeF i o u)
    (<+<) :: Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r

Play around with this code by downloading
[Pipe.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/Pipe.lhs).
(You'll need
[Fun.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/Fun.lhs)
from last time in the same directory).

