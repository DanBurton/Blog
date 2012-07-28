Last time, we enhanced the `await` primitive,
making it aware of when the upstream pipe returned a value.
However, the change forced us to modify
our style of programming. This is not necessarily
a bad thing, but today, we'll recover
the old capabilities we had by adding
a new primitive: `abort`.
This will restore the ability for upstream
pipes to shut down the pipeline.

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeAbort where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> import Fun ((:&:)(..), (:|:)(..))
> 
> import Data.Void (Void)
> import Control.Monad (when, forever)
> import Control.Monad.Trans.Class (lift)


Functors
--------------------------------------------------

We finally revisit our fourth old friend,
the Empty functor, and give it the name `Abort`.
Recall that the Empty functor allows us
to short circuit computation without
providing any other information.

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

> type YieldThen o = Yield o :&: Then
> type AwaitU i u = Await i :&: Await u

With our shiny new Abort functor in hand,
we just union it in with the other options
in a `PipeF`.

> type PipeF i o u = YieldThen o :|: AwaitU i u :|: Abort
> type Pipe i o u  = FreeT (PipeF i o u)
> 
> type Producer o   = Pipe () o    ()
> type Consumer i u = Pipe i  Void u
> type Pipeline     = Pipe () Void ()


Working with PipeF
--------------------------------------------------

I've defined `:|:` to be left-associative,
which means that we can simply union another thing
onto the right side, and wrap everything we used to have
in a big `L`. This change is reflected in the lifting functions.

> liftYield :: YieldThen o next ->        PipeF i o u next
> liftYield = L . L
> 
> liftAwait :: AwaitU i u next ->         PipeF i o u next
> liftAwait = L . R
> 
> liftAbort :: Abort next ->              PipeF i o u next
> liftAbort = R
> 
> yieldF :: o -> next ->                  PipeF i o u next
> yieldF o next = liftYield $ Yield o :&: Then next
> 
> awaitF :: (i -> next) -> (u -> next) -> PipeF i o u next
> awaitF f g = liftAwait $ Await f :&: Await g
> 
> abortF :: PipeF i o u next
> abortF = liftAbort Abort

I've added a smart constructor for `Abort`, which is entirely straightforward.
We'll need to add another branch to our `pipeCase` construct.
`pipeCase` must be prepared with a default `a`, because `Abort` provides
absolutely no information.

> pipeCase :: FreeF (PipeF i o u) r next
>          ->                                a  -- Abort
>          -> (r                          -> a) -- Return
>          -> (o -> next                  -> a) -- Yield
>          -> ((i -> next) -> (u -> next) -> a) -- Await
>                                         -> a
> pipeCase (Wrap (R Abort))
>   k _ _ _ = k
> pipeCase (Return r)
>   _ k _ _ = k r
> pipeCase (Wrap (L (L (Yield o :&: Then next))))
>   _ _ k _ = k o next
> pipeCase (Wrap (L (R (Await f :&: Await g))))
>   _ _ _ k = k f g


Pipe primitives
--------------------------------------------------

> awaitE :: Monad m => Pipe i o u m (Either u i)
> awaitE  = liftF $ awaitF Right Left
> 
> yield :: Monad m => o -> Pipe i o u m ()
> yield b = liftF $ yieldF b ()
> 
> abort :: Monad m => Pipe i o u m r
> abort = liftF abortF

Our primitives remain unchanged.
We add the `abort` primitive; notice that it is
polymorphic in its return type. In fact, it's
polymorphic in, well, *everything*. Its complete
lack of information means that it can be used
to fill *any* hole that has the shape of a Pipe.


Pipe composition
--------------------------------------------------

The type of pipe composition does not change with this modification.

> (<+<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> p1 <+< p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1

Everywhere we used `pipeCase`, we'll need to add the extra branch
for the `Abort` case. If the downstream pipe aborted, then
everything upstream is discarded, as it is when downstream returns a value.

>   {- Abort  -} (abort)               -- upstream discarded
>   {- Return -} (\r      -> return r) -- upstream discarded
>   {- Yield  -} (\o next -> wrap $ yieldF o (next <+< p2))
>   {- Await  -} (\f1 g1  -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2

If the upstream pipe aborted, then downstream is forcibly aborted as well,
meaning that the downstream pipe is discarded.

>     {- Abort  -} (abort)             -- downstream discarded

When the upstream pipe
produces a result, we'll give that result to the appropriate downstream handler.
We used to then regurgitate the same result over and over to the downstream pipe
every time it asked.

    [haskell]
    {- Return -} (\u' -> g1 u' <+< return u')


We're going to change that behavior now. Instead,
we will cause an `abort` if downstream ever `await`s after receiving the
upstream's final result.

>     {- Return -} (\u'     -> g1 u' <+< abort) -- downstream gets one last shot

The rest remains as before.

>     {- Yield  -} (\o next -> f1 o  <+< next)
>     {- Await  -} (\f2 g2  -> wrap $ awaitF (\i -> p1' <+< f2 i)
>                                            (\u -> p1' <+< g2 u)))


If `idP` is like multiplying by 1, then
`abort` is like multiplying by 0. Sort of.
As always, downstream drives, so if the upstream pipe is `abort`,
but the downstream never consults upstream, then downstream can
continue on its merry way for as long as it wants.

$$\forall p \in Pipe, abort \circ p \equiv abort$$

$$\forall p \in Producer, p \circ abort \equiv p$$

Note that our current `Producer` type is not strong enough
to actually guarantee this: it only restricts the input type
to `()`, rather than preventing awaits altogether.

> (>+>) :: Monad m => Pipe i i' u m u' -> Pipe i' o u' m r -> Pipe i o u m r
> (>+>) = flip (<+<)

> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

Now that a pipeline might abort at any time without a result,
we need to adjust `runPipe` to take this possibility of failure
into account. Instead of producing `m r`, we'll produce a `m (Maybe r)`.
If the pipeline is aborted, `Nothing` is produced as the result.

> runPipe :: Monad m => Pipeline m r -> m (Maybe r)
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Abort  -} (return Nothing)
>   {- Return -} (\r       -> return $ Just r)
>   {- Yield  -} (\_o next -> runPipe next)
>   {- Await  -} (\f _g    -> runPipe $ f ())


Some basic pipes
-------------------------------------------------

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

We can still write the same pipes as before.
`awaitForever` never asks for input after it gets the upstream result,
so it will never be the source of an abort.

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

Bringing back the good(?) stuff
-------------------------------------------------

Now that we are equipped with both the `abort` and `awaitE` primitives,
we can reproduce the good ol' `await` that we had from before:

> await :: Monad m => Pipe i o u m i
> await = awaitE >>= \ex -> case ex of
>   Left _u -> abort
>   Right i -> return i

That means that we can resurrect the old style of pipe programming
right alongside the new style:

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

This code is identical to the code we had from part 1.
Neat, huh? Notice how these versions of `id`, `filter`, etc,
do *not* bear the restriction that $u = r$. However,
they doesn't behave exactly the same as before,
because `abort` causes the pipeline to fail *without any result*.

    [ghci]
    runPipe $ (printer >> return "not hijacked") <+< return "hijacked"
      Just "not hijacked"
    runPipe $ (oldPrinter >> return "not hijacked") <+< return "hijacked"
      Nothing


Next time
-------------------------------------------------

We've granted upstream pipes the power to abort downstream pipes
that await on them, but is this too much power? What if downstream
doesn't *want* to go down? Next time, we'll up the granularity of control
once more by allowing downstream pipes to provide a handler for the case
of an aborted upstream. Once we have that in place, we can start thinking
about guaranteed finalizers.

You can play with this code for yourself by downloading
[PipeAbort.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeAbort.lhs).
