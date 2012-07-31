Back in part 5, we added the ability to attach arbitrary finalizers
to pipes. But when those finalizers actually ran was purely mechanical:
when any given pipe finished, it would run all upstream finalizers, and then
its own. This behavior can sometimes delay the finalization of an upstream 
pipe, if the downstream pipe stops awaiting but continues running
and possibly yielding.

This time, we'll add the `close` primitive,
which will allow the programmer to indicate that a pipe
will never `await` again. This should possibly be named
`unsafeClose`, because in this implementation,
we will not use the type system to enforce this guarantee.

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeClose where
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

We'll not add a new functor this time;
we'll just reuse `Then` to indicate
the "rest" of the computation after a pipe closes its input end.

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

> type LeftoverThen l = Leftover l :&: Then
> type YieldThen o m  = Yield o :&: Finalize m :&: Then
> type AwaitU i u     = Await i :&: Await u :&: Then
> type Close          = Then

Our `PipeF` type has certainly grown! Remember when it used to be
just `Await i :|: YieldThen o`? At least we're not adding yet another
type parameter this time.

> type PipeF l i o u m =  YieldThen o m
>                     :|: AwaitU i u
>                     :|: Abort
>                     :|: LeftoverThen l
>                     :|: Close
> type Pipe l i o u m r = FreeT (PipeF l i o u m) m r
> 
> type Producer   o   m r = Pipe Void () o    () m r
> type Consumer l i u m r = Pipe l    i  Void u  m r
> type Pipeline       m r = Pipe Void () Void () m r


Working with PipeF
--------------------------------------------------

We update the lifts, the smart constructors, and `pipeCase` as usual.

> liftYield :: YieldThen o m next ->              PipeF l i o u m next
> liftYield = L . L . L . L
> 
> liftAwait :: AwaitU i u next ->                 PipeF l i o u m next
> liftAwait = L . L . L . R
> 
> liftAbort :: Abort next ->                      PipeF l i o u m next
> liftAbort = L . L . R
> 
> liftLeftover :: LeftoverThen l next ->          PipeF l i o u m next
> liftLeftover = L . R
> 
> liftClose :: Close next ->                      PipeF l i o u m next
> liftClose = R

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
> 
> closeF :: next ->                               PipeF l i o u m next
> closeF next = liftClose $ Then next

> pipeCase :: FreeF (PipeF l i o u m) r next
>  ->                                        a  -- Abort
>  -> (r                                  -> a) -- Return
>  -> (next                               -> a) -- Close
>  -> (l -> next                          -> a) -- Leftover
>  -> (o -> m () -> next                  -> a) -- Yield
>  -> ((i -> next) -> (u -> next) -> next -> a) -- Await
>                                         -> a
> pipeCase (Wrap (L (L (R Abort))))
>   k _ _ _ _ _ = k
> pipeCase (Return r)
>   _ k _ _ _ _ = k r
> pipeCase (Wrap (R (Then next)))
>   _ _ k _ _ _ = k next
> pipeCase (Wrap (L (R (Leftover l :&: Then next))))
>   _ _ _ k _ _ = k l next
> pipeCase (Wrap (L (L (L (L (Yield o :&: Finalize m :&: Then next))))))
>   _ _ _ _ k _ = k o m next
> pipeCase (Wrap (L (L (L (R (Await f :&: Await g :&: Then next))))))
>   _ _ _ _ _ k = k f g next


Pipe primitives
--------------------------------------------------

We add a new primitive, as usual.

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
> 
> close :: Monad m => Pipe l i o u m ()
> close = liftF $ closeF ()


Pipe composition
--------------------------------------------------

> (<+<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <+< p2 = composeWithFinalizer pass p1 p2

> (<?<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <?< p2 = composeWithFinalizer unreachable p1 p2

Now all uses of `pipeCase` must have an additional branch for `Close`.

> composeWithFinalizer :: Monad m => m ()
>                  -> Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> composeWithFinalizer finalizeUpstream p1 p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1
>   {- Abort  -} (         lift finalizeUpstream >> abort)
>   {- Return -} (\r ->    lift finalizeUpstream >> return r)
>   {- Close  -} (\next -> lift finalizeUpstream >> (wrap $ closeF (next <+< abort)))

The very reason that we made the Close option was so that
the upstream pipe could be finalized early.
Once we do that, what do we compose with `next`?
We could compose it with `p2`, but that would be *very* unsafe,
since `p2`'s finalizers have been run. Imagine if `p2` were reading
from a file, then we close the file, then ask `p2` to keep reading!
So instead, we compose with abort. Recall that earlier we asserted that
right-composing a Producer with `abort` was the same as the identity function:

$\forall p \in Producer, p \circ abort \equiv p$

If it is indeed true that the downstream pipe will never `await` again,
then we can rest assured that `next <+< abort` will behave as we desire.

>   {- L-over -} (\l _next -> absurd l)
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
>     {- Close  -} (\next -> wrap $ closeF (p1' <?< next))

Suppose that the upstream pipe `close`s its input end.
If we have reached this point, then the up-upstream finalizers
have already been run, so we need not worry about it.
Upstream still has control, so we'll compose `next` with the
unreachable finalizer, as we do for similar situations.

>     {- L-over -} (\l next -> wrap $ leftoverF l (p1' <?< next))
>     {- Yield  -} (\o newFinalizer next ->
>                       let (<*<) = composeWithFinalizer newFinalizer
>                       in f1 o <*< next)
>     {- Await  -} (\f2 g2 onAbort2 -> wrap $ awaitF
>                           (\i -> p1' <?< f2 i)
>                           (\u -> p1' <?< g2 u)
>                           (      p1' <?< onAbort2)))



> (>+>) :: Monad m => Pipe l i i' u m u' -> Pipe Void i' o u' m r -> Pipe l i o u m r
> (>+>) = flip (<+<)
> 
> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

At the level of a pipeline, the `close` operation
is meaningless, since it shouldn't be awaiting anyways.
When we `runPipe` on a `Close`, therefore, we will simply move on
to the next computation.

> runPipe :: Monad m => Pipeline m r -> m (Maybe r)
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Abort  -} (                  return Nothing)
>   {- Return -} (\r             -> return $ Just r)
>   {- Close  -} (\next          -> runPipe next)
>   {- L-over -} (\l _next       -> absurd l)
>   {- Yield  -} (\o _fin _next  -> absurd o)
>   {- Await  -} (\f _g _onAbort -> runPipe $ f ())


Getting rid of leftovers
-------------------------------------------------

The adjustment to `injectLeftovers` is interesting:
once we close the input end, what should we do with leftovers?
Discard them, since we promised not to look at them? Or keep them,
since it doesn't hurt the upstream pipe if we look at the stuff
that we already acquired from it.

> injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r
> injectLeftovers = go [] where
>   go ls p = FreeT $ do
>     x <- runFreeT p
>     runFreeT $ pipeCase x
>     {- Abort  -} (abort)
>     {- Return -} (\r -> return r)
>     {- Close  -} (\next -> wrap $ closeF (go [] next))

In the name of garbage collection, I say dump them.
This is reflected by the recursive call ignoring `ls` and instead
passing in an empty list.

>     {- L-over -} (\l next -> go (l:ls) next)
>     {- Yield  -} (\o fin next -> wrap $ yieldF o fin (go ls next))
>     {- Await  -} (\f g onAbort -> case ls of
>       [] -> wrap $ awaitF (go [] . f) (go [] . g) (go [] onAbort)
>       l : ls' -> go ls' (f l))


Adding finalizers to a pipe
-------------------------------------------------

`Close` is always an intermediate step of a pipe
(even if the next step is merely `return ()`),
so when revisiting `cleanupP`, we need only make sure
that the cleanup procedures are passed on to the next computation.

> cleanupP :: Monad m => m () -> m () -> m () -> Pipe l i o u m r
>          -> Pipe l i o u m r
> cleanupP abortFinalize selfAbortFinalize returnFinalize = go where
>   go p = FreeT $ do
>     x <- runFreeT p
>     runFreeT $ pipeCase x
>     {- Abort  -} (      lift selfAbortFinalize >> abort)
>     {- Return -} (\r -> lift returnFinalize    >> return r)
>     {- Close  -} (\next -> wrap $ closeF (go next))
>     {- L-over -} (\l next -> wrap $ leftoverF l (go next))
>     {- Yield  -} (\o finalizeRest next -> wrap $
>                         yieldF o (finalizeRest >> abortFinalize) (go next))
>     {- Await  -} (\f g onAbort -> wrap $
>                         awaitF (go . f) (go . g) (go onAbort))


Playing with our new primitive
-------------------------------------------------

Here's a simple example using (essentially) printf debugging
to illustrate the code execution path. See if you can guess
the output of `runPipe $ exampleConsumer <+< exampleProducer`
before looking at it.

> exampleProducer :: Producer Int IO ()
> exampleProducer = finallyP (putStrLn "End producer") $ do
>   lift $ putStrLn "Begin producer"
>   lift $ putStrLn "Producer yielding"
>   yield 1
>   lift $ putStrLn "Producer done yielding"
>   pass

> exampleConsumer :: Consumer Void Int () IO ()
> exampleConsumer = finallyP (putStrLn "End consumer") $ do
>   lift $ putStrLn "Begin consumer"
>   lift $ putStrLn "Consumer awaiting"
>   _ <- await
>   lift $ putStrLn "Consumer done awaiting"
>   close
>   lift $ putStrLn "Consumer continues"
>   pass

    [ghci]
    runPipe $ exampleConsumer <+< exampleProducer
      Begin consumer
      Consumer awaiting
      Begin producer
      Producer yielding
      Consumer done awaiting
      End producer
      Consumer continues
      End consumer
      Just ()

As you can see, `close` caused the producer's finalizer to be run immediately.
What would the output look like if the consumer didn't `close`?


Safety
-------------------------------------------------

Our `close` primitive gives programmers a convenient way
to indicate that a pipe's upstream should be finalized,
but it is completely up to the programmer to make sure
that `close` is used in a safe way, that is, that it is not
followed by an `await`. We gave pipes the behavior of
`abort`ing in such circumstances, which is a decent choice,
but can we do better?

Control.Frame from the `pipes` package provides a
different (though similar) implementation of pipes that uses
indexed monads to solve this problem. If you `close` a frame,
then it is a *type error* to `await` afterwards.
This has obvious type-safety benefits, but at the cost of
using a relatively new concept that has little syntactic sugar
support; see
[Control.Pipe.Tutorial](http://hackage.haskell.org/packages/archive/pipes/latest/doc/html/Control-Frame-Tutorial.html)
for details on how this technique works.


Next time
-------------------------------------------------

We've come a long ways from the simplicity of Control.Pipe.
Next time, I'll take away `abort` and `close`, and what we have left
will be fairly similar to the current state of Data.Conduit.
I'll guide you through some of the `conduit` source code and
observe which choices were made, and attempt to explain why.


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

> idMsg :: String -> Pipe l i i u IO u
> idMsg str = finallyP (putStrLn str) idP
> 
> testPipeR :: Monad m => Pipe Void i o u m r -> m (Maybe r)
> testPipeR p = runPipe $ (await >> abort) <+< p <+< abort
> 
> testPipeL :: Monad m => Pipe Void Int o () m r -> m (Maybe r)
> testPipeL p = runPipe $ (await >> await >> abort) <+< take' 1 <+< p <+< fromList [1 ..]
> 
> testPipe :: Monad m => Pipe Void Int o () m r -> m (Maybe (r, [o]))
> testPipe p = runPipe $ runP <+< p <+< fromList [1..]
> 
> take' :: Monad m => Int -> Pipe l i i u m ()
> take' 0 = pass
> take' n = (await >>= yield) >> take' (pred n)

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
[PipeClose.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeClose.lhs).

