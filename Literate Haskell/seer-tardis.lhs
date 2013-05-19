    [BLOpts]
    title = "Two implementations of Seers"

Last time, we implemented a bowling game scorer
by using a Tardis. If you aren't yet familiar with
the Tardis's interface, then I recommend you check out
[the explanation on Hackage](http://hackage.haskell.org/packages/archive/tardis/0.3.0.0/doc/html/Control-Monad-Tardis.html).
(tl;dr it's a State monad with get and put,
except there are two streams of state,
one forwards and one backwards,
so there are four operations: `getPast`, `getFuture`,
`sendPast`, and `sendFuture`.)

Today, we'll take another large step in the esoteric drection,
and implement a Seer by using a Tardis.
Why, you ask? My response: why not?
There may be some deep motivating reasons for you to study this,
but I don't pretend to know what those might be.

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DoRec #-}
> {-# OPTIONS_GHC -Wall #-}

> import Control.Applicative (Applicative)
> import Control.Monad (liftM)
> import Control.Monad.Fix (MonadFix, mfix)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Tardis
> import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
> import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
> import Data.Monoid

What is a Seer?
======================================================================

A *seer* is someone that foretells the future.^[[Wiktionary > seer]]
But how do seers know the future? Suppose you are writing a novel,
and you want to devise a semi-believable "system" for how seers work.
What would the rules be?

[Wiktionary > seer]: http://en.wiktionary.org/w/index.php?title=seer&oldid=18654193

Well, rule number one for me is that in a legitimate system,
all seers must agree about the future. If different seers predict
different outcomes for the same future period, then there is
reason to doubt such a system. I decided that in *my* seer system,
all seers see "the whole universe". **All seers see the same thing**,
regardless of when or where in space and time they decide to "see" it.

Now, where does this information come from? Are there separate people
that send information to these seers? My first idea was that the
seer system could be a network of seers, and all information comes
from within the network itself. All seers are therefore required
to provide accurate information about their "present" in order to tap
into the reservoir of mystical information about their past and future.

We therefore come to the main operation that I have devised for seers.

    [haskell]
    contact :: Monoid w => w -> Seer w

A seer provides their worldview in exchange for the grand worldview.
The "whole" world should be of the form `past <> present <> future`,
where `present` is whatever value is provided as the argument to
`contact`.

Remember when I wondered about whether those that "see" the universe
and those that "send" information about the universe might be different
people? It turns out that we can easily write operations `see` and `send`
in terms of `contact`. Or, alternatively, given `see` and `send`,
we can easily write `contact` in terms of those.

> class (Monad m, Monoid w) => MonadSeer w m | m -> w where
>   see :: m w
>   send :: w -> m ()
>   contact :: w -> m w
>   
>   see = contact mempty
>   send w = contact w >> return ()
>   contact w = send w >> see

I've created a typeclass for the Seer interface, because
we are going to implement a seer in two different ways.


Seer in terms of a Tardis
======================================================================

The `Tardis` allows us to both get and send messages to both the
past and future. Given the timey-wimey nature of seers,
a tardis seems like the perfect candidate for implementing them.

> newtype SeerT w m a = SeerT { unSeerT :: TardisT w w m a }
>                     deriving (Functor, Applicative, Monad, MonadFix)

A single `contact` consists of a seer getting in touch with
both the past and the future. It seems only fair that this seer
should share with the future his newfound knowledge of the past, and
with the past his knowledge of the future. The past is inquiring
the present about its (the past's) future, which includes both
the present and the future, or in other words `present <> future`.
The future is inquiring the present about its (the future's) past,
which includes both the present and the past, or in other words,
`past <> present`. The result of the `contact` is the whole universe,
spanning all of time, in other words, `past <> present <> future`.
In all cases, we want to make sure to keep things in "chronological" order.

Did you follow all of that? In short, information from the past
should be sent forwards to the future, and information from the
future should be sent backwards to the past. We can encode
this flow of information easily using the Tardis operations:

> instance (Monoid w, MonadFix m) => MonadSeer w (SeerT w m) where
>   contact present = SeerT $ do
>     rec past <- getPast
>         sendPast (present <> future)
>         sendFuture (past <> present)
>         future <- getFuture
>     return (past <> present <> future)

Now, in order to "run" a seer operation, all we have to do
is provide `mempty` at both ends of the time continuum,
and run the tardis as usual.

> runSeerT :: (MonadFix m, Monoid w) => SeerT w m a -> m a
> runSeerT = flip evalTardisT (mempty, mempty) . unSeerT

Here is a dumb example demonstrating how it works.

> dumbExample :: MonadSeer [Int] m => m [Int]
> dumbExample = do
>   world1 <- see
>   send [world1 !! 2]
>   send [1]
>   world2 <- see
>   send [world2 !! 1]
>   world3 <- see
>   return world3

    [ghci]
    runSeerT dumbExample
      [1,1,1]

It is actually unnecessary to `see` more than once,
since it is always the unchanging truth of `past <> present <> future`.
The following is equivalent:

    [haskell]
    dumbExample = do
      world <- see
      send [world !! 2]
      send [1]
      send [world !! 1]
      return world

Seer in terms of a Reader/Writer
======================================================================

The astute observer should have noticed an odd similarity between
`see` and `ask`, `send` and `tell`. They embody practically the
same concept! The only nuance is that when you `ask`, what you will
get is everything that you have `tell`'d, and everything you will `tell`.
It turns out that this is quite easy to write in terms of the
`Reader` and `Writer` monad transformers, which happen to be
instances of MonadFix.

> newtype RWSeerT w m a = RWSeerT { unRWSeerT :: ReaderT w (WriterT w m) a }
>                       deriving (Functor, Applicative, Monad, MonadFix)

As I said before, `see` is simply `ask`, while `send` is simply `tell`.
We merely lift and wrap the operations as necessary to keep
the type system happy:

> instance (Monoid w, Monad m) => MonadSeer w (RWSeerT w m) where
>   see = RWSeerT ask
>   send w = RWSeerT (lift (tell w))

Now, to run a Seer built on top of a Reader/Writer pair, all we have
to do is feed the results of the `Writer` straight back into the `Reader`.
We accomplish this via `mfix`.

> runRWSeerT :: (Monoid w, MonadFix m) => RWSeerT w m a -> m a
> runRWSeerT (RWSeerT rwma) = liftM fst $
>   mfix (\ ~(_, w) -> runWriterT (runReaderT rwma w))

Here is a dumb example demonstrating that it works

    [ghci]
    runRWSeerT dumbExample
      [1,1,1]

So why use a Tardis?
======================================================================

For fun, obviously!

More seriously, notice that we can "run" SeerT differently, depending
on whether we implemented it with Tardis or with Reader/Writer.
With Tardis, we can supply "bookends", the *further* past and the
*further* future.

> runSeerTWith :: (MonadFix m, Monoid w) => w -> w -> SeerT w m a -> m a
> runSeerTWith past future = flip evalTardisT (future, past) . unSeerT

Exercise:
Predict the output of `runSeerTWith [10, 11, 12] [16, 17, 18] dumbExample`.

Whereas with the reader/writer pair, we can fool the seers
by giving them a false reality.

> runRWSeerTWith :: (Monoid w, Monad m) => w -> RWSeerT w m a -> m a
> runRWSeerTWith falseReality (RWSeerT rwma) = liftM fst $
>   runWriterT (runReaderT rwma falseReality)

Exercise:
Predict the output of `runRWSeerTWith [10, 11, 12] dumbExample`.

What the ramifications of these are, I really don't know.
I just follow the types, lean on the laziness, and things
just seem to work in Haskell, even mystical things like time travel
and seers.

[Download this code and play with it!](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/seer-tardis.lhs)
Don't forget to `cabal install tardis` first.