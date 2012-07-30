One important use case of the Conduit library
is parsing. In order to perform useful parsing,
we need to be able to occasionally consume "too much"
input, and then put the "leftovers" back into the
input stream, as if they had never been consumed.

Today, we will extend the `Pipe` type yet again,
creating a new primitive, `leftover`,
comparable to that of Data.Conduit.

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeLeftover where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> import Fun ((:&:)(..), (:|:)(..))
> 
> import Data.Void (Void, absurd)
> import Control.Monad (when, forever)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Resource (MonadResource, allocate, release)


Functors
--------------------------------------------------
 
We'll create yet another synonym for Const,
this time called `Leftover`.
 
> newtype Then next = Then next            -- Identity
> newtype Yield o next = Yield o           -- Const
> newtype Await i next = Await (i -> next) -- Fun
> data Abort next = Abort                  -- Empty
> newtype Finalize m next = Finalize (m ()) -- Const
> newtype Leftover l next = Leftover l     -- Const

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
> 
> instance Functor (Finalize m) where
>   fmap _f (Finalize m) = Finalize m
> 
> instance Functor (Leftover l) where
>   fmap _f (Leftover l) = Leftover l
> 
> pass :: Monad m => m ()
> pass = return ()
> 
> unreachable :: Monad m => m ()
> unreachable = error "You've reached the unreachable finalizer"


The Pipe type
--------------------------------------------------

The usage of `leftover` will be much like that of `yield`,
we supply a value, and then carry on with our computation.
We will therefore bundle `Leftover` with `Then`,
as we did with `YieldThen`.

> type LeftoverThen l = Leftover l :&: Then
> type YieldThen o m  = Yield o :&: Finalize m :&: Then
> type AwaitU i u     = Await i :&: Await u :&: Then

PipeF and Pipe will acquire a new type parameter `l`
which indicates the type of leftovers that a given pipe
will supply.

> type PipeF l i o u m =  YieldThen o m :|: AwaitU i u
>                     :|: Abort         :|: LeftoverThen l
> type Pipe l i o u m r = FreeT (PipeF l i o u m) m r
> 
> type Producer   o   m r = Pipe Void () o    () m r
> type Consumer l i u m r = Pipe l    i  Void u  m r
> type Pipeline       m r = Pipe Void () Void () m r


Working with PipeF
--------------------------------------------------

Our lifting functions will be adjusted as usual:
the pre-existing ones acquire another `L`,
while the new one gets an `R`.

> liftYield :: YieldThen o m next ->              PipeF l i o u m next
> liftYield = L . L . L
> 
> liftAwait :: AwaitU i u next ->                 PipeF l i o u m next
> liftAwait = L . L . R
> 
> liftAbort :: Abort next ->                      PipeF l i o u m next
> liftAbort = L . R
> 
> liftLeftover :: LeftoverThen l next ->          PipeF l i o u m next
> liftLeftover = R

We add a smart constructor `leftoverF` in similar fashion
to the ones we have already.

> yieldF :: o -> m () -> next ->                  PipeF l i o u m next
> yieldF o m next = liftYield $ Yield o :&: Finalize m :&: Then next
> 
> awaitF :: (i -> next) -> (u -> next) -> next -> PipeF l i o u m next
> awaitF f g next = liftAwait $ Await f :&: Await g :&: Then next
> 
> abortF ::                                       PipeF l i o u m next
> abortF = liftAbort Abort
> 
> leftoverF :: l -> next ->                       PipeF l i o u m next
> leftoverF l next = liftLeftover $ Leftover l :&: Then next

And finally we add another branch to `pipeCase`.

> pipeCase :: FreeF (PipeF l i o u m) r next
>  ->                                        a  -- Abort
>  -> (r                                  -> a) -- Return
>  -> (l -> next                          -> a) -- Leftover
>  -> (o -> m () -> next                  -> a) -- Yield
>  -> ((i -> next) -> (u -> next) -> next -> a) -- Await
>                                         -> a
> pipeCase (Wrap (L (R Abort)))
>   k _ _ _ _ = k
> pipeCase (Return r)
>   _ k _ _ _ = k r
> pipeCase (Wrap (R (Leftover l :&: Then next)))
>   _ _ k _ _ = k l next
> pipeCase (Wrap (L (L (L (Yield o :&: Finalize m :&: Then next)))))
>   _ _ _ k _ = k o m next
> pipeCase (Wrap (L (L (R (Await f :&: Await g :&: Then next)))))
>   _ _ _ _ k = k f g next


Pipe primitives
--------------------------------------------------

Now that we're old pros with `liftF`,
the `leftover` primitive is a breeze.

> tryAwait :: Monad m =>      Pipe l i o u m (Either (Maybe u) i)
> tryAwait = liftF $ awaitF Right (Left . Just) (Left Nothing)
> 
> yield :: Monad m => o ->    Pipe l i o u m ()
> yield b = liftF $ yieldF b pass ()
> 
> abort :: Monad m =>         Pipe l i o u m r
> abort = liftF abortF
> 
> leftover :: Monad m => l -> Pipe l i o u m ()
> leftover l = liftF $ leftoverF l ()


Getting rid of leftovers
-------------------------------------------------

Being able to specify leftovers is one thing,
but how do we interpret that? What does it *mean* when a pipe
supplies leftovers? The "obvious" meaning is that
the rest of the pipe computation should have that leftover value
available to it the next time it awaits.

Let's write an interpreter that will "inject" leftovers
into a pipe, making them available to the pipe's own `await`s.
The given pipe must therefore bear the restriction that
the leftover type is the same as the input type.
The resultant pipe will contain no `leftover` constructs,
and so it can therefore be polymorphic in that type parameter.

The situation might arise where two leftovers are supplied in a row.
What should we do then? Discard the old and keep the new?
If we keep both, then which order should they be supplied back
to the subsequent `await`s?

Recall that `Pipe`s are a form of stream processing.
Suppose we represent the stream as a queue. `await` and `yield`
are like the operations `dequeue` (taking from the front of a queue)
and `enqueue` (adding to the back of a queue) respectively.
The idea of "leftovers" is that we accidentally took "too much",
and we want to reverse our actions. The logical conclusion, therefore,
is that the `leftover` operation should "push" a value
back onto the *front* of the queue.

> injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r
> injectLeftovers = go [] where

Our "queue" is going to be represented by a list.
An empty list means "please refer to the actual stream".
A nonempty list means "I have these values that I took from the stream;
please pretend like they're still there."

>   go ls p = FreeT $ do
>     x <- runFreeT p
>     runFreeT $ pipeCase x
>     {- Abort  -} (abort)
>     {- Return -} (\r -> return r)
>     {- L-over -} (\l next -> go (l:ls) next)

When we encounter a `leftover` statement,
we have yet another value we took from the stream,
and we'd like to "put it back". We therefore cons it onto the front.

>     {- Yield  -} (\o fin next -> wrap $ yieldF o fin (go ls next))
>     {- Await  -} (\f g onAbort -> case ls of
>       [] -> wrap $ awaitF (go [] . f) (go [] . g) (go [] onAbort)
>       l : ls' -> go ls' (f l))

When we encounter an `await`, there are two possibilities:
either we have an empty list, and we need to refer to the actual stream,
or we have a nonempty list, and we can just take the top value.
"Referring to the actual stream" translates to creating another `await` construct,
while "just taking the top value" translates to invoking the `f` callback
with the `l` value.

Pipe composition
--------------------------------------------------

The question arises: how are we supposed to compose
two pipes that both might supply leftovers?
There are a few possibilities.

If we allow them both to supply leftovers, then
should we discard the leftovers from one pipe or the other?
Perhaps the resultant pipe could simply have an `Either` union
of the two types of leftovers.

The other option is to disallow leftovers from one or both
pipes upon composing them. If we disallow leftovers from one pipe,
then the resultant pipe will have the leftover type of the other one.
If we disallow leftovers from both pipes, then there is no way
for their composition to produce leftovers.

Given the nature of `injectLeftovers`, which associates leftovers
with the "input" type `i`, and given that the resultant input type `i`
comes from the upstream pipe, the logical choice seems to be
to allow leftovers from the upstream pipe, but not the downstream pipe.
We "disallow" leftovers by specifying that the type of leftovers
for the downstream pipe is `Void`. It is impossible to construct
a value of type `Void`, unless it is an infinite loop or an exception.

> (<+<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <+< p2 = composeWithFinalizer pass p1 p2

> (<?<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <?< p2 = composeWithFinalizer unreachable p1 p2

All we have to change in pipe composition is
to add branches for `leftover` whenever we `pipeCase`.

> composeWithFinalizer :: Monad m => m ()
>                  -> Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> composeWithFinalizer finalizeUpstream p1 p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1
>   {- Abort  -} (      lift finalizeUpstream >> abort)
>   {- Return -} (\r -> lift finalizeUpstream >> return r)
>   {- L-over -} (\l _next -> absurd l)

Since the downstream pipe has a leftover type of `Void`,
we can use `absurd` to assert that this branch should never happen.

>   {- Yield  -} (\o finalizeDownstream next ->
>                       let (<*<) = composeWithFinalizer finalizeUpstream
>                       in wrap $ yieldF o
>                           (finalizeUpstream >> finalizeDownstream)
>                           (next <*< p2))
>   {- Await  -} (\f1 g1 onAbort1 -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2
>     {- Abort  -} (    onAbort1 <+< abort) -- downstream recovers
>     {- Return -} (\u' -> g1 u' <+< abort) -- downstream recovers
>     {- L-over -} (\l next -> wrap $ leftoverF l (p1' <?< next))

If the upstream pipe produced a leftover, then we'll keep it.
Since upstream still has control, there is no reason to expect
that the finalizer we provide to pipe composition will be used,
so we'll use the `unreachable` one.
Note that the types make no guarantees about `unreachable`,
rather, it is my own assertion. I arrived at the conclusion
that the provided finalizer for this location would be unreachable
by reasoning about the code, but I see no convenient way to encode
or enforce this it in the type system.

>     {- Yield  -} (\o newFinalizer next ->
>                       let (<*<) = composeWithFinalizer newFinalizer
>                       in f1 o <*< next)
>     {- Await  -} (\f2 g2 onAbort2 -> wrap $ awaitF
>                           (\i -> p1' <?< f2 i)
>                           (\u -> p1' <?< g2 u)
>                           (      p1' <?< onAbort2)))


> (>+>) :: Monad m => Pipe l i i' u m u' -> Pipe Void i' o u' m r -> Pipe l i o u m r
> (>+>) = flip (<+<)

> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

Given that a pipeline cannot reasonably use
`yield` or `leftover`, since those types are constrained to `Void`,
let's again make use of `absurd` to discharge us
of the obligation to provide code for those branches.

> runPipe :: Monad m => Pipeline m r -> m (Maybe r)
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Abort  -} (                  return Nothing)
>   {- Return -} (\r             -> return $ Just r)
>   {- L-over -} (\l _next       -> absurd l)
>   {- Yield  -} (\o _fin _next  -> absurd o)
>   {- Await  -} (\f _g _onAbort -> runPipe $ f ())


Adding finalizers to a pipe
-------------------------------------------------

There is little to say about the changes here.
The `leftover` construct promises that there is is
a `next` pipe, so we simply attach the cleanup actions
to that next pipe, and that's it.

> cleanupP :: Monad m => m () -> m () -> m () -> Pipe l i o u m r
>          -> Pipe l i o u m r
> cleanupP abortFinalize selfAbortFinalize returnFinalize = go where
>   go p = FreeT $ do
>     x <- runFreeT p
>     runFreeT $ pipeCase x
>     {- Abort  -} (      lift selfAbortFinalize >> abort)
>     {- Return -} (\r -> lift returnFinalize    >> return r)
>     {- L-over -} (\l next -> wrap $ leftoverF l (go next))
>     {- Yield  -} (\o finalizeRest next -> wrap $
>                         yieldF o (finalizeRest >> abortFinalize) (go next))
>     {- Await  -} (\f g onAbort -> wrap $
>                         awaitF (go . f) (go . g) (go onAbort))


Play time
-------------------------------------------------

Let's give leftovers a spin!

    [ghci]
    :set -XNoMonomorphismRestriction
    let p = leftover "hello" >> leftover "world" >> idP
    runPipe $ execP <+< injectLeftovers p <+< fromList ["the", "end"]
      Just ["world","hello","the","end"]

Note that this is a horrible abuse of `leftover`.
The concept of leftovers is that they are made as a way for you
to put back onto the stream that which you have taken off.

Here's perhaps a more sensible use of `leftover`:
FORTH-style programming!

> swap :: Monad m => Pipe i i o u m ()
> swap = do 
>   i1 <- await
>   i2 <- await
>   leftover i1
>   leftover i2

> dup :: Monad m => Pipe i i o u m ()
> dup = do
>   i <- await
>   leftover i
>   leftover i

    [ghci]
    :set -XNoMonomorphismRestriction
    let p = injectLeftovers (swap >> dup >> idP)
    runPipe $ execP <+< p <+< fromList [1 .. 5]
      Just [2,2,1,3,4,5]

Perhaps the simplest use of leftovers is the ability
to "peek" at the value coming next without consuming it.

> peekE :: Monad m => Pipe i i o u m (Either u i)
> peekE = awaitE >>= \ex -> case ex of
>   Left u  -> return (Left u)
>   Right i -> leftover i >> return (Right i)



Next time
-------------------------------------------------

I initially planned for the series to end right around here,
but I have decided to extend it to touch on two more topics.
Next time, we will extend our Pipe type with a new primitive, `close`,
allowing it to signal that it is finished consuming input, so that upstream
finalizers can be run as soon as possible.
After that, we'll take away `close` and `abort`, and compare
the result to Data.Conduit, which has neither of those two features.
Whether that is a "good" or "bad" thing is up for you to decide,
but I'll try to point out a few of the trade-offs.

Convenience combinators
-------------------------------------------------

> finallyP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> finallyP finalize = cleanupP finalize finalize finalize
> 
> catchP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> catchP finalize = cleanupP finalize finalize pass
> 
> successP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> successP finalize = cleanupP pass pass finalize

> bracketP :: MonadResource m => IO a -> (a -> IO ()) -> (a -> Pipe l i o u m r)
>          -> Pipe l i o u m r
> bracketP create destroy mkPipe = do
>   (key, val) <- lift $ allocate create destroy 
>   finallyP (release key) (mkPipe val)


Some basic pipes
-------------------------------------------------

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

> awaitE :: Monad m => Pipe l i o u m (Either u i)
> awaitE = tryAwait >>= \emx -> case emx of
>   Left Nothing  -> abort
>   Left (Just u) -> return $ Left u
>   Right i       -> return $ Right i
> 
> awaitForever :: Monad m => (i -> Pipe l i o u m r) -> Pipe l i o u m u
> awaitForever f = go where
>   go = awaitE >>= \ex -> case ex of
>     Left u  -> return u
>     Right i -> f i >> go
> 
> pipe :: Monad m => (i -> o) -> Pipe l i o u m u
> pipe f = awaitForever $ yield . f
> 
> idP :: Monad m => Pipe l i i u m u
> idP = pipe id
> 
> filterP :: Monad m => (i -> Bool) -> Pipe l i i u m u
> filterP test = awaitForever $ \x -> when (test x) (yield x)
> 
> printer :: Show i => Consumer l i u IO u
> printer = awaitForever $ lift . print

> runP :: Monad m => Consumer l i u m (u, [i])
> runP = awaitE >>= \ex -> case ex of
>   Left  u -> return (u, [])
>   Right i -> runP >>= \ ~(u, is) -> return (u, i:is)
> 
> evalP :: Monad m => Consumer l i u m u
> evalP = fst `fmap` runP
> 
> execP :: Monad m => Consumer l i u m [i]
> execP = snd `fmap` runP
> 
> fold :: Monad m => (r -> i -> r) -> r -> Consumer l i u m r
> fold f = go where
>   go r = awaitE >>= \ex -> case ex of
>     Left _u -> return r
>     Right i -> go $! f r i

> await :: Monad m => Pipe l i o u m i
> await = awaitE >>= \ex -> case ex of
>   Left _u -> abort
>   Right i -> return i
> 
> oldPipe :: Monad m => (i -> o) -> Pipe l i o u m r
> oldPipe f = forever $ await >>= yield . f
> 
> oldIdP :: Monad m => Pipe l i i u m r
> oldIdP = oldPipe id
> 
> oldFilterP :: Monad m => (i -> Bool) -> Pipe l i i u m r
> oldFilterP test = forever $ await >>= \x -> when (test x) (yield x)
> 
> oldPrinter :: Show i => Consumer l i u IO r
> oldPrinter = forever $ await >>= lift . print


You can play with this code for yourself by downloading
[PipeLeftover.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeLeftover.lhs).

