Last time, we introduced the `abort` primitive,
which restored the power to write pipes a la Control.Pipe.
However, the power for upstream pipes to force
those downstream to abort is perhaps too much.
This time, we're going to give downstream pipes
the ability to recover from an upstream abort.

The changes made to the code from last time are minimal in this post;
in fact it barely qualifies as worthy of its own post.
However, once we've seen the changes there is something important
that I would like to point out. If you've been following along
with the series, then you can just skim over most of the code.

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeRecover where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> import Fun ((:&:)(..), (:|:)(..))
> 
> import Data.Void (Void)
> import Control.Monad (when, forever)
> import Control.Monad.Trans.Class (lift)


Functors
--------------------------------------------------

Nothing new here.

> newtype Then next = Then next            -- Identity
> newtype Yield o next = Yield o           -- Const
> newtype Await i next = Await (i -> next) -- Fun
> data Abort next = Abort                  -- Empty

> instance Functor Then where
>   fmap f (Then next) = Then (f next)
>
> instance Functor (Yield o) where
>   fmap _f (Yield o) = Yield o
> 
> instance Functor (Await i) where
>   fmap f (Await g) = Await (f . g)
> 
> instance Functor Abort where
>   fmap _f Abort = Abort


The Pipe type
--------------------------------------------------

We will grant downstream pipes the ability to handle an abort
in a similar fashion to the way we granted them the ability
to handle upstream results: by extending our `Await`
with another callback. This time, there is no input,
so we simply use the `Then` functor:

> type YieldThen o = Yield o :&: Then
> type AwaitU i u = Await i :&: Await u :&: Then

> type PipeF i o u = YieldThen o :|: AwaitU i u :|: Abort
> type Pipe i o u  = FreeT (PipeF i o u)
> 
> type Producer o   = Pipe () o    ()
> type Consumer i u = Pipe i  Void u
> type Pipeline     = Pipe () Void ()


Working with PipeF
--------------------------------------------------

Little changes in this section.
We merely enhance `awaitF` to accept the third input,
and `pipeCase` to similarly handle the extra callback.

> liftYield :: YieldThen o next ->                PipeF i o u next
> liftYield = L . L
> 
> liftAwait :: AwaitU i u next ->                 PipeF i o u next
> liftAwait = L . R
> 
> liftAbort :: Abort next ->                      PipeF i o u next
> liftAbort = R
> 
> yieldF :: o -> next ->                          PipeF i o u next
> yieldF o next = liftYield $ Yield o :&: Then next
> 
> awaitF :: (i -> next) -> (u -> next) -> next -> PipeF i o u next
> awaitF f g next = liftAwait $ Await f :&: Await g :&: Then next
> 
> abortF :: PipeF i o u next
> abortF = liftAbort Abort

> pipeCase :: FreeF (PipeF i o u) r next
>  ->                                        a  -- Abort
>  -> (r                                  -> a) -- Return
>  -> (o -> next                          -> a) -- Yield
>  -> ((i -> next) -> (u -> next) -> next -> a) -- Await
>                                         -> a
> pipeCase (Wrap (R Abort))
>   k _ _ _ = k
> pipeCase (Return r)
>   _ k _ _ = k r
> pipeCase (Wrap (L (L (Yield o :&: Then next))))
>   _ _ k _ = k o next
> pipeCase (Wrap (L (R (Await f :&: Await g :&: Then next))))
>   _ _ _ k = k f g next


Pipe primitives
--------------------------------------------------

`awaitE` is no longer sufficient for our needs,
we need to extend our `await` primitive yet again.
Where before we promised `Either u i`, we must now add
a third possibility: upstream abort. There is no additional
information associated with this, so let's use a `Maybe`:
`Nothing` will signal that an `abort` has occurred upstream.

We therefore have 3 choices: `Maybe (Either u i)`,
`Either (Maybe u) i`, or `Either u (Maybe i)`.
I like the second choice, because we can preserve `Left`
as signaling upstream termination, whether that be
an `abort` or a `return`.

> tryAwait :: Monad m => Pipe i o u m (Either (Maybe u) i)
> tryAwait = liftF $ awaitF Right (Left . Just) (Left Nothing)
> 
> yield :: Monad m => o -> Pipe i o u m ()
> yield b = liftF $ yieldF b ()
> 
> abort :: Monad m => Pipe i o u m r
> abort = liftF abortF


Pipe composition
--------------------------------------------------

The changes to pipe composition are minimal:
when downstream awaits on upstream,
and upstream aborts, then make use of the
provided callback.

> (<+<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> p1 <+< p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1
>   {- Abort  -} (abort)               -- upstream discarded
>   {- Return -} (\r      -> return r) -- upstream discarded
>   {- Yield  -} (\o next -> wrap $ yieldF o (next <+< p2))
>   {- Await  -} (\f1 g1 onAbort1 -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2

v This is the part that changed

>     {- Abort  -} (        onAbort1 <+< abort) -- downstream recovers

^ This is the part that changed

>     {- Return -} (\u'     -> g1 u' <+< abort) -- downstream recovers
>     {- Yield  -} (\o next -> f1 o  <+< next)
>     {- Await  -} (\f2 g2 onAbort2 -> wrap $ awaitF
>                                            (\i -> p1' <+< f2 i)
>                                            (\u -> p1' <+< g2 u)
>                                            (      p1' <+< onAbort2)))


> (>+>) :: Monad m => Pipe i i' u m u' -> Pipe i' o u' m r -> Pipe i o u m r
> (>+>) = flip (<+<)

> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

When running a pipe, the new callback makes no difference.

> runPipe :: Monad m => Pipeline m r -> m (Maybe r)
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Abort  -} (                  return Nothing)
>   {- Return -} (\r             -> return $ Just r)
>   {- Yield  -} (\_o next       -> runPipe next)
>   {- Await  -} (\f _g _onAbort -> runPipe $ f ())


Some basic pipes
-------------------------------------------------

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

We can easily write `awaitE` from our last post:
we simply give up our chance of recovering from an `abort`
in the `Left Nothing` case.

> awaitE :: Monad m => Pipe i o u m (Either u i)
> awaitE = tryAwait >>= \emx -> case emx of
>   Left Nothing  -> abort
>   Left (Just u) -> return $ Left u
>   Right i       -> return $ Right i

From there, all of the code from the last post is possible.
(Skim past if you've seen it already.)

> awaitForever :: Monad m => (i -> Pipe i o u m r) -> Pipe i o u m u
> awaitForever f = go where
>   go = awaitE >>= \ex -> case ex of
>     Left u  -> return u
>     Right i -> f i >> go
> 
> pipe :: Monad m => (i -> o) -> Pipe i o u m u
> pipe f = awaitForever $ yield . f
> 
> idP :: Monad m => Pipe i i u m u
> idP = pipe id
> 
> filterP :: Monad m => (i -> Bool) -> Pipe i i u m u
> filterP test = awaitForever $ \x -> when (test x) (yield x)
> 
> printer :: Show i => Consumer i u IO u
> printer = awaitForever $ lift . print

> runP :: Monad m => Consumer i u m (u, [i])
> runP = awaitE >>= \ex -> case ex of
>   Left  u -> return (u, [])
>   Right i -> runP >>= \ ~(u, is) -> return (u, i:is)
> 
> evalP :: Monad m => Consumer i u m u
> evalP = fst `fmap` runP
> 
> execP :: Monad m => Consumer i u m [i]
> execP = snd `fmap` runP
> 
> fold :: Monad m => (r -> i -> r) -> r -> Consumer i u m r
> fold f = go where
>   go r = awaitE >>= \ex -> case ex of
>     Left _u -> return r
>     Right i -> go $! f r i

> await :: Monad m => Pipe i o u m i
> await = awaitE >>= \ex -> case ex of
>   Left _u -> abort
>   Right i -> return i
> 
> oldPipe :: Monad m => (i -> o) -> Pipe i o u m r
> oldPipe f = forever $ await >>= yield . f
> 
> oldIdP :: Monad m => Pipe i i u m r
> oldIdP = oldPipe id
> 
> oldFilterP :: Monad m => (i -> Bool) -> Pipe i i u m r
> oldFilterP test = forever $ await >>= \x -> when (test x) (yield x)
> 
> oldPrinter :: Show i => Consumer i u IO r
> oldPrinter = forever $ await >>= lift . print

Primitives for recovering from an abort
-------------------------------------------------

We can enhance any pipe by giving it
new instructions whenever it or its upstream connection aborts.
We'll create a new primitive called `recover` to accomplish this.
Anything that is written with `pipeCase`, or that uses either
`wrap` or `liftF`, I call a "primitive".
If it can be written in terms of other primitives,
without using `pipeCase`, `liftF`, or `wrap` (and obviously
without digging into a pipe's internals any other way)
then I don't consider it to be a "primitive".

> recover :: Monad m => Pipe i o u m r -> Pipe i o u m r -> Pipe i o u m r
> originalP `recover` newP = FreeT $ do
>   x <- runFreeT originalP
>   runFreeT $ pipeCase x
>   {- Abort  -} (newP)
>   {- Return -} (\r -> return r)
>   {- Yield  -} (\o next -> wrap $ yieldF o (next `recover` newP))
>   {- Await  -} (\f g onAbort -> let go p = p `recover` newP in
>                            wrap $ awaitF (go . f) (go . g) (go onAbort))
> 
> recoverWith :: Monad m => Pipe i o u m r -> r -> Pipe i o u m r
> p `recoverWith` r = p `recover` return r

Here's a little ghci session comparing and contrasting
uses of "old pipe" programming and recovery with "new pipes".

    [ghci]
    let idThen r = oldIdP `recoverWith` r
    runPipe $ runP <+< idThen 5 <+< fromList [1..3]
      Just (5,[1,2,3])
    runPipe $ runP <+< oldIdP <+< fromList [1..3]
      Nothing
    runPipe $ runP <+< fmap (const 5) oldIdP <+< fromList [1..3]
      Nothing
    runPipe $ runP <+< fmap (const 5) idP <+< fromList [1..3]
      Just (5,[1,2,3])

Next time, We'll be doing something very similar to this,
in order to add arbitrary finalizers to pipes.
Go back and review the implementation of `recover`.
Do you understand how it works?
Could we have written `recover` using the code from last time?
How does this version of `recover` make use of the change we made
to the `PipeF` functor?


Maybe r versus Abort
-------------------------------------------------

Now that we've given downstream pipes the ability to
recover from an upstream abort again, aren't we back
where we were at two blog posts ago before we ever had `abort`?

Consider back when we didn't have `abort`.
What if we simply constrained `Pipe` to have a
result type of `Maybe r`?

    [haskell]
    type Part2PipeF i o u = YieldThen o :|: AwaitU i u
    type Part2Pipe i o u  = FreeT (PipeF i o u)

    type Pipe i o u m r = Part2Pipe i o u m (Maybe r)

(Also consider that `MaybeT (Part2Pipe i o u m) r` is isomorphic to
`Part2Pipe i o u m (Maybe r)`, so the following applies similarly
to sticking a `MaybeT` on top.)

There would be a few consequences.

 * The `awaitE` primitive would produce a `Either (Maybe u) r`
   instead of `Either u r`.
 * `runPipe` would produce `m (Maybe r)`
   instead of plain old `m r`.
 * The pipe that trivially returns `Nothing` could be completely polymorphic,
   and would be able to fill any hole shaped like a Pipe.
 * Pipe composition would have to deal with `Maybe`s.
   It could constantly return an upstream result of `Nothing`
   after the first time of returning a meaningful result.

These should all sound extremely familiar, because
they are nearly identical to the code in this very file!


What's the point of Abort?
-------------------------------------------------

Recall that last time, we added `abort` with the motivation
that we wanted to write pipes the old way like we used to
with Control.Pipe. By allowing upstream pipes to abort
the pipeline, we were able to write code using the blissful `await`
primitive that simply relied on the automatic termination properties
of pipes.

Notice how Conduit (referring to version 0.5.x) does not provide
any "untainted" await primitive. The ones it provides are

    [haskell]
    await :: Pipe l i o u m (Maybe i)
    awaitE :: Pipe l i o u m (Either u i)
    awaitForever :: Monad m => (i -> Pipe l i o r m r') -> Pipe l i o r m r

Always await with a caveat. But this allows
the conduit version of `runPipe` to *not* deal with
`Maybe`s. It's a trade-off: where do you want to deal with the possibility
that a pipe has shut down? If you aren't forced to deal with it
inside your pipes code, then you are instead forced to deal with it
at the level of `runPipe`. Or, you can just revert to the old-school
Control.Pipe way, and retrieve the result from *whichever* pipe
in a pipeline produces the first result.

What if we want the best of both worlds, and want
pipe composition without constraining result types to be the same,
but nevertheless want to provide an *additional* parameter `e`
that any pipe can abort to, so that `runPipe` is guaranteed
to have *something* instead of the possibility of `Nothing`.
Sounds like a job for `Either`!
Paolo, in his `pipes-core` fork of `pipes`,
provides an `EitherT e` on top of (essentially) `Part2Pipe`,
but with the additional constraint that `e` be the same as `u`!

As you can see, when designing a pipes library,
there are a lot of potential trade-offs;
it's not very black-and-white which ones are the "best"
ones to choose. We'll just have to keep gaining practical experience
with and proving properties of the various options.
Keep an eye on `pipes` and `pipes-core`, because it seems that
Paolo and Gabriel are starting to agree more than usual!
`conduit` has undergone massive changes since its inception around
8 months ago (while nevertheless surprisingly retaining much of the same API);
I wouldn't be surprised to see these three packages
merge into one within the next year or two.

Next time
-------------------------------------------------

I'll continue from here with `abort` still intact,
but at the end of the series we will remove it,
just to show how our end result is identical to conduit.
At least I think it will be. We'll see when we get there.

This time we created the `recover` combinator,
which affords us some amount of fault tolerance.
Next time, we'll add proper pipe finalization hooks.
By combining these with `ResourceT`, we can provide
convenient, guaranteed, exception-safe resource finalization.

    newtype Finalize m next = Finalize (m ())
    type YieldThen o m = Yield o :&: Finalize m :&: Then

You can play with this code for yourself by downloading
[PipeRecover.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeRecover.lhs).

