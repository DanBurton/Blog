Last time we introduced abort recovery,
allowing downstream pipes to recover from an `abort`.
We were able to write the `recover` combinator,
which could attach a recovery pipe to *any* other pipe.

Today, we'll look at a different aspect of pipe termination:
finalizers. As we have discussed before, downstream pipes
may discard upstream pipes when they are done with them,
whether the upstream pipe has returned a result or not.
That pipe may have unfinished business: for example,
open file handles or database connections that need to be closed.
We'd like to be able to dictate arbitrary actions
which will always be performed before a pipe is discarded.

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeFinalize where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> import Fun ((:&:)(..), (:|:)(..))
> 
> import Data.Void (Void)
> import Control.Monad (when, forever)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Resource (MonadResource, allocate, release)

Functors
--------------------------------------------------

We'll add another Const synonym, `Finalize`.
This one is parameterized by a monad `m`,
and contains an arbitrary action in that monad: `m ()`.

> newtype Then next = Then next            -- Identity
> newtype Yield o next = Yield o           -- Const
> newtype Await i next = Await (i -> next) -- Fun
> data Abort next = Abort                  -- Empty
> newtype Finalize m next = Finalize (m ()) -- Const

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

There will be times when a finalizer is expected,
but we have none to give, and we don't want anything to occur.
We'll just `return ()` in those cases, so how about a nicer name
for that idiom.

> pass :: Monad m => m ()
> pass = return ()

There will also come a time when we must supply a finalizer,
but we never expect it to be used. We *could* use `pass` for that, too,
but instead, let's use an exploding bomb with a message attached.
We'd like to be informed if the unreachable is reached.

> unreachable :: Monad m => m ()
> unreachable = error "You've reached the unreachable finalizer!"

The Pipe type
--------------------------------------------------

We will attach the `Finalize` information to the
`Yield` information. That way, when upstream yields to
downstream, and downstream decides to discard upstream,
downstream can use the latest finalizer it acquired from upstream.

That was a lot of up and down so reread that sentence a few times
until it becomes clear. It sounds childish, but I find these things
tend to make more sense when I wave my hand left when I read "downstream"
and right when I read "upstream". It's also more fun when you add
other gestures for verbs.

> type YieldThen o m = Yield o :&: Finalize m :&: Then
> type AwaitU i u    = Await i :&: Await u :&: Then

> type PipeF i o u m = YieldThen o m :|: AwaitU i u :|: Abort
> type Pipe i o u m r = FreeT (PipeF i o u m) m r

Pay special attention to how `Pipe` is defined here.
It makes sure that `m` is the same `m` given to both the `PipeF` functor
*and* to `FreeT`. See if you can explain why this is significant.

> type Producer o   m r = Pipe () o    () m r
> type Consumer i u m r = Pipe i  Void u  m r
> type Pipeline     m r = Pipe () Void () m r


Working with PipeF
--------------------------------------------------

The `yieldF` smart constructor is extended appropriately,
as is `pipeCase`.

> liftYield :: YieldThen o m next ->              PipeF i o u m next
> liftYield = L . L
> 
> liftAwait :: AwaitU i u next ->                 PipeF i o u m next
> liftAwait = L . R
> 
> liftAbort :: Abort next ->                      PipeF i o u m next
> liftAbort = R
> 
> yieldF :: o -> m () -> next ->                  PipeF i o u m next
> yieldF o fin next = liftYield $ Yield o :&: Finalize fin :&: Then next
> 
> awaitF :: (i -> next) -> (u -> next) -> next -> PipeF i o u m next
> awaitF f g next = liftAwait $ Await f :&: Await g :&: Then next
> 
> abortF :: PipeF i o u m next
> abortF = liftAbort Abort

> pipeCase :: FreeF (PipeF i o u m) r next
>  ->                                        a  -- Abort
>  -> (r                                  -> a) -- Return
>  -> (o -> m () -> next                  -> a) -- Yield
>  -> ((i -> next) -> (u -> next) -> next -> a) -- Await
>                                         -> a
> pipeCase (Wrap (R Abort))
>   k _ _ _ = k
> pipeCase (Return r)
>   _ k _ _ = k r
> pipeCase (Wrap (L (L (Yield o :&: Finalize fin :&: Then next))))
>   _ _ k _ = k o fin next
> pipeCase (Wrap (L (R (Await f :&: Await g :&: Then next))))
>   _ _ _ k = k f g next


Pipe primitives
--------------------------------------------------

The `yield` primitive should have no finalizer attached,
so we just give it `pass` for that slot.

> tryAwait :: Monad m => Pipe i o u m (Either (Maybe u) i)
> tryAwait = liftF $ awaitF Right (Left . Just) (Left Nothing)
> 
> yield :: Monad m => o -> Pipe i o u m ()
> yield b = liftF $ yieldF b pass ()
> 
> abort :: Monad m => Pipe i o u m r
> abort = liftF abortF


Pipe composition
--------------------------------------------------

The type of composition again remains the same,
however, we now need to keep track of an additional argument:
the most recent upstream finalizer. We can still keep `(<+<)`,
but this will just be a synonym for the new composition function,
supplying it the empty finalizer, `pass`.

> (<+<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> p1 <+< p2 = composeWithFinalizer pass p1 p2

It will also be convenient to define composition using the
unreachable finalizer. You'll see why momentarily.

> (<?<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> p1 <?< p2 = composeWithFinalizer unreachable p1 p2

> composeWithFinalizer :: Monad m => m ()
>                  -> Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> composeWithFinalizer finalizeUpstream p1 p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1

And now the fun begins. Wherever we used to recursively invoke `(<+<)`,
we now need to consider: do we need to retain the current upstream finalizer?
To maintain *some* similarity with previous code,
whenever we need to invoke `composeWithFinalizer` recursively,
we'll let-bind a new operator `(<*<)`, which will have some particular
finalizer baked in: which one depends on each situation as we will soon see.
(Recall that we also have `<+<` and `<?<` at our disposal,
which have `pass` and `unreachable` finalizers baked in, respectively.)

>   {- Abort  -} (      lift finalizeUpstream >> abort)
>   {- Return -} (\r -> lift finalizeUpstream >> return r)

Upon reaching a downstream `abort` or `return`,
we are going to discard the upstream pipe, so we must run
the finalizer. Since `Pipe` is an instance of `MonadTrans`
(by virtue of being a synonym for a `FreeT`), we can simply `lift`
the finalizer into a pipe, and then sequence it (`>>`) with
the appropriate result.

>   {- Yield  -} (\o finalizeDownstream next ->
>                       let (<*<) = composeWithFinalizer finalizeUpstream
>                       in wrap $ yieldF o
>                           (finalizeUpstream >> finalizeDownstream)
>                           (next <*< p2))

If the downstream pipe is yielding a result,
then both the upstream *and* the downstream pipe are at peril
of being discarded by a pipe further down the line.
Fortunately, the `yield` construct provides an appropriate finalizer for `p1`,
and we *already* have an appropriate finalizer for `p2`,
so we'll just bundle them together in a new `yield` construct.
But which of the two should we run first? I chose to run the
upstream finalizer first, and I'll explain why later in this post.

In the event that control returns to our downstream pipe,
we need not worry about `finalizeDownstream`,
because `p1` is once again in control. Therefore,
when we compose `next` with `p2`, we only bundle in `finalizeUpstream`.

>   {- Await  -} (\f1 g1 onAbort1 -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2
>     {- Abort  -} (    onAbort1 <+< abort) -- downstream recovers
>     {- Return -} (\u' -> g1 u' <+< abort) -- downstream recovers

In the event that downstream is `await`ing, control transfers upstream.
If the upstream pipe is `return`ing or `abort`ing,
then we no longer need to care about finalizing it: it has already
finalized itself by this point. Therefore, we can use the regular
`(<+<)` operator for these cases, and forget about the
`finalizeUpstream` we used to have.

>     {- Yield  -} (\o newFinalizer next ->
>                       let (<*<) = composeWithFinalizer newFinalizer
>                       in f1 o <*< next)

If downstream is `await`ing, and upstream is `yield`ing,
then that means the upstream pipe has provided a `newFinalizer`
to use instead of the old one.

>     {- Await  -} (\f2 g2 onAbort2 -> wrap $ awaitF
>                           (\i -> p1' <?< f2 i)
>                           (\u -> p1' <?< g2 u)
>                           (      p1' <?< onAbort2)))

When both `p1` and `p2` are awaiting, well *that* is an interesting case.
Consider: `p2` is transferring control *further* upstream. When
control comes back to `p2`, `p1` will still be `await`ing.
The only way that control will transfer back down to `p1` is if
`p2` decides to `abort`, `return`, or `yield`.
If it `abort`s or `return`s, then it will have finalized itself.
If it `yield`s, then it will supply a brand new finalizer.

So the question is, when we re-compose `p1` with either `f2 i`,
`g2 u`, or `onAbort2`, what finalizer should we use?
From what I just said in the previous paragraph, it should be apparent that
no matter what finalizer we provide here, it will *never be used*.
So we'll just hand it the exploding bomb: `unreachable`.

> (>+>) :: Monad m => Pipe i i' u m u' -> Pipe i' o u' m r -> Pipe i o u m r
> (>+>) = flip (<+<)

> infixr 9 <+<
> infixr 9 >+>

Phew, we made it through again. Finalization is tricky:
each case requires careful thought and analysis in order to make sure
you are doing the right thing. But did we really do the right thing
by using `unreachable`? Are you sure? Review the code, and think about it.
Why did we use `<+<` for upstream Return and Abort cases instead of `<?<`?


Running a pipeline
--------------------------------------------------

A yielded finalizer makes no difference to `runPipe`.

> runPipe :: Monad m => Pipeline m r -> m (Maybe r)
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Abort  -} (                  return Nothing)
>   {- Return -} (\r             -> return $ Just r)
>   {- Yield  -} (\_o _fin next  -> runPipe next)
>   {- Await  -} (\f _g _onAbort -> runPipe $ f ())


Adding finalizers to a pipe
-------------------------------------------------

Well being able to compose pipes with finalizers is well and good,
but how do we add finalizers to pipes in the first place?
Let's create a new pipe primitive: `cleanupP`.

> cleanupP :: Monad m => m () -> m () -> m () -> Pipe i o u m r
>          -> Pipe i o u m r
> cleanupP abortFinalize selfAbortFinalize returnFinalize = go where
>   go p = FreeT $ do
>     x <- runFreeT p
>     runFreeT $ pipeCase x

By inspecting a given pipe via `pipeCase`, we can attach
finalizers in three distinct places.

>     {- Abort  -} (      lift selfAbortFinalize >> abort)

Any pipe can decide to `abort`. For example,
in a previous blog post, we created the `await` pseudo-primitive,
which voluntarily aborts if an upstream pipe aborts or returns.

>     {- Return -} (\r -> lift returnFinalize    >> return r)

Any pipe can decide to `return`. This is another opportunity for finalization.

>     {- Yield  -} (\o finalizeRest next -> wrap $
>                         yieldF o (finalizeRest >> abortFinalize) (go next))

Finally, any pipe can be discarded when it yields control to a downstream pipe.
A yield construct may already have finalizers associated with it,
so when we add our new one, we'll just tack it on at the end.
We could have just as easily decided to put the new finalizer first;
we'll discuss that decision momentarily.

Notice that we also recursively apply this finalizer to the `next`
pipe after `yield`.
That's because if control returns to this pipe from downstream,
then we still want to finalize it later.

>     {- Await  -} (\f g onAbort -> wrap $
>                         awaitF (go . f) (go . g) (go onAbort))

We anticipate each possibility in the `await` case,
and recursively apply the finalizer to all of them.


More convenient finalization combinators
-------------------------------------------------

`cleanupP` is too general to be useful. Let's create some convenience
combinators to handle typical needs.

> finallyP :: Monad m => m () -> Pipe i o u m r -> Pipe i o u m r
> finallyP finalize = cleanupP finalize finalize finalize

If we want a given finalizer run *no matter what*,
then just use it for all 3 possibilities.

> catchP :: Monad m => m () -> Pipe i o u m r -> Pipe i o u m r
> catchP finalize = cleanupP finalize finalize pass

If we only want a finalizer to run if something "goes wrong",
then we simply `pass` on the `return` option.

> successP :: Monad m => m () -> Pipe i o u m r -> Pipe i o u m r
> successP finalize = cleanupP pass pass finalize

Conversely, we may only want a finalizer to run in the absence of "problems",
so we pass on both "problem" cases.

> bracketP :: MonadResource m => IO a -> (a -> IO ()) -> (a -> Pipe i o u m r)
>          -> Pipe i o u m r
> bracketP create destroy mkPipe = do
>   (key, val) <- lift $ allocate create destroy 
>   finallyP (release key) (mkPipe val)

`ResourceT` provides `allocate` and `release` to help you deal with
finalizers, even in the face of thrown exceptions.
We can make good use of this by `lift`ing `allocate` into a Pipe,
and then adding the corresponding `release` as a finalizer!


How do we know which finalizer comes first?
-------------------------------------------------

I've defined a few quick-n-dirty functions here to help us observe
the behavior of pipe finalization.

> idMsg :: String -> Pipe i i u IO u
> idMsg str = finallyP (putStr $ str ++ " ") idP
> 
> take' :: Monad m => Int -> Pipe i i u m ()
> take' 0 = pass
> take' n = (await >>= yield) >> take' (pred n)

`testPipeR` will test what happens when `abort` comes from upstream.

> testPipeR :: Monad m => Pipe i o u m r -> m (Maybe r)
> testPipeR p = runPipe $ (await >> abort) <+< p <+< abort

`testPipeL` will test what happens when `abort` comes from downstream.

> testPipeL :: Monad m => Pipe Int o () m r -> m (Maybe r)
> testPipeL p = runPipe $ (await >> await >> abort) <+< take' 1 <+< p <+< fromList [1 ..]

`testPipe` will test what happens when `abort` comes from within the pipe itself.

> testPipe :: Monad m => Pipe Int o () m r -> m (Maybe (r, [o]))
> testPipe p = runPipe $ runP <+< p <+< fromList [1..]

> examplePipe :: Pipe Int Int u IO ()
> examplePipe = idMsg "one" <+< take' 5 <+< idMsg "two" <+< idMsg "three"

Let's take this for a spin.

    [ghci]
    testPipeR examplePipe
      three two one Nothing
    testPipeL examplePipe
      three two one Nothing
    testPipe examplePipe
      three two one Just ((),[1,2,3,4,5])

Well that's boring. In each case the finalizers run in order
from upstream to downstream: "three two one".
But it's boring on purpose: the way that I have defined
for finalizers to behave is that if you are a pipe,
and *your* finalizer is running, you can safely assume
that any pipes *upstream* of you have already been finalized.

I encourage you to
[download this code](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeFinalize.lhs)
, and mess with it (requires
[Fun.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/Fun.lhs)
as well, tested on GHC 7.4.1).
What happens when you switch the order of finalizers
on line 204 (pipe composition)? What happens when you switch
the order of finalizers on line 317 (cleanupP)?
What if you switch both? Can you think of any circumstances
when you'd want a pipe's finalizer to run *before* pipes upstream of it
are finalized? You can use this command to run the ghci examples
and see the difference between the expected output and the actual output:

    $ BlogLiterately -g PipeFinalize.lhs > test.html && firefox test.html

Next time
-------------------------------------------------

The subtleties of finalization provide us a lot to think about.
There is again room for many possible implementations, but logic
and seeking consistent behavior can help us narrow the possibilities,
and Haskell's type system often guides us to the "obvious" solution.

Next time, we'll tackle the "leftovers" feature, using the same style
as `conduit`. I'll try to point out all of the areas where different
implementations are possible, because I feel that the decisions are
less clear for leftovers than for previous features.


Some basic pipes
-------------------------------------------------

Here's all of those pipes from previous posts.
They remain unchanged: you can ignore the new finalizer
capability that we added and go right along writing pipes
just like you did before we added this feature.

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield
> 
> awaitE :: Monad m => Pipe i o u m (Either u i)
> awaitE = tryAwait >>= \emx -> case emx of
>   Left Nothing  -> abort
>   Left (Just u) -> return $ Left u
>   Right i       -> return $ Right i
> 
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
> 
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


You can play with this code for yourself by downloading
[PipeFinalize.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeFinalize.lhs).

