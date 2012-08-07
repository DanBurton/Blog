In this series, we started with the simplest of Pipe implementations,
and added features one by one until we reached Conduit-like functionality.
Today, we'll strip away the `abort` and `close` features not present in Conduit
(the former might be considered a misfeature, though without using
indexed monads it is a necessity for the latter), and compare the results.
There is one major difference, which I believe illustrates a serious flaw
in both implementations. I will illustrate this issue at the end of the post.

For now, walk with me through some of our old code,
as we compare it side-by-side with the code from Data.Conduit.Internal
(from conduit-0.5.2.2).

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> 
> module PipeConduit where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> 
> import Data.Void (Void, absurd)
> import Control.Monad.Trans.Class
> import Control.Monad.Trans.Resource (MonadResource, allocate, release)
> 
> import qualified Data.Conduit as C
> import qualified Data.Conduit.List as C
> 
> import qualified Control.Frame as F
> import Control.IMonad.Trans (liftU)
> import Control.IMonad.Restrict (foreverR, mapMR_, (!>=), (!>))

Helpers
--------------------------------------------------

> pass :: Monad m => m ()
> pass = return ()
> 
> unreachable :: Monad m => m ()
> unreachable = error "You've reached the unreachable finalizer"


The Pipe type
--------------------------------------------------

I've decided for this post to revamp and re-arrange the `PipeF` type.
Sans `close` and `abort`, and with mild re-arrangement of
the order of fields for given constructors,
you should be able to tell that it is
identical to the `PipeF` type we have worked with before.

> data PipeF l i o u m next
>   = Yield next (m ()) o
>   | Await (i -> next) (u -> next)
>   | Leftover next l

The `Functor` instance for this type is entirely mechanical,
based on its components. We could have just as easily used
`-XDeriveFunctor` and arrived at the same instance.

> instance Functor (PipeF l i o u m) where
>   fmap h (Yield next fin o) = Yield (h next) fin o
>   fmap h (Await f g) = Await (h . f) (h . g)
>   fmap h (Leftover next l) = Leftover (h next) l

> type Pipe l i o u m r =
>   FreeT (PipeF l i o u m) m r

Now compare this with the `Pipe` type from `Data.Conduit.Internal`.
I've rearranged the order of the lines of code, and removed comments,
but otherwise the code is untouched.

    [haskell]
    data Pipe l i o u m r =
        HaveOutput (Pipe l i o u m r) (m ()) o
      | NeedInput (i -> Pipe l i o u m r) (u -> Pipe l i o u m r)
      | Leftover (Pipe l i o u m r) l

      | Done r
      | PipeM (m (Pipe l i o u m r))

If you are comfortable in your grasp of the Free monad transformer,
then you should be able to see that our two representations are equivalent.
The `Done` and `PipeM` constructors are analogous to `Return` and `Wrap`,
while `HaveOutput`, `NeedInput`, and `Leftover` are analogous to
`Yield`, `Await`, and `Leftover` respectively.

I'm going to define some synonyms for `FreeT` and `runFreeT`
to help illustrate the similarities in implementation.

> pipeM :: m (FreeF (PipeF l i o u m) r (Pipe l i o u m r))
>       -> Pipe l i o u m r
> pipeM m = FreeT m
> 
> runPipeM :: Pipe l i o u m r
>          -> m (FreeF (PipeF l i o u m) r (Pipe l i o u m r))
> runPipeM (FreeT m) = m

For the `Conduit` implementation, you could imagine analogous methods
that would allow us to write the Conduit code
in similar fashion to what you've seen here before.

    pipeM :: Monad m => m (Pipe l i o u m r) -> Pipe l i o u m r
    pipeM m = PipeM m

    runPipeM :: Monad m => Pipe l i o u m r -> m (Pipe l i o u m r)
    runPipeM (PipeM m) = m >>= runPipeM
    runPipeM p = return p

> type Producer   o   m r = Pipe Void () o    () m r
> type Consumer l i u m r = Pipe l    i  Void u  m r
> type Pipeline       m r = Pipe Void () Void () m r


Working with PipeF
--------------------------------------------------

I'll keep using `pipeCase` to maintain similarity with previous code,
although without the functor composition cruft, it's really
not that bad to just use direct pattern matching.

I've upgraded to `transformers-free-1.0` which means that
`Return` and `Wrap` are now called `Pure` and `Free` respectively.

> pipeCase :: FreeF (PipeF l i o u m) r next
>  -> (r                          -> a) -- Return
>  -> (next -> l                  -> a) -- Leftover
>  -> (next -> m () -> o          -> a) -- Yield
>  -> ((i -> next) -> (u -> next) -> a) -- Await
>                                 -> a
> pipeCase (Pure r)
>   k _ _ _ = k r
> pipeCase (Free (Leftover next l))
>   _ k _ _ = k next l
> pipeCase (Free (Yield next fin o))
>   _ _ k _ = k next fin o
> pipeCase (Free (Await f g))
>   _ _ _ k = k f g


Pipe primitives
--------------------------------------------------

The Free monad transformer allows us to write the primitives
using the convenient `liftF` combinator.

> awaitE :: Monad m =>        Pipe l i o u m (Either u i)
> awaitE = liftF $ Await Right Left
> 
> yield :: Monad m => o ->    Pipe l i o u m ()
> yield b = liftF $ Yield () pass b
> 
> leftover :: Monad m => l -> Pipe l i o u m ()
> leftover l = liftF $ Leftover () l

The Conduit implementation is a bit crufty in comparison,
but obviously identical.

    [haskell]
    awaitE :: Pipe l i o u m (Either u i)
    awaitE = NeedInput (Done . Right) (Done . Left)

    yield :: Monad m => o -> Pipe l i o u m ()
    yield = HaveOutput (Done ()) (return ())

    leftover :: l -> Pipe l i o u m ()
    leftover = Leftover (Done ())


Pipe composition
--------------------------------------------------

> (<+<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <+< p2 = composeWithFinalizer pass p1 p2

> (<?<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <?< p2 = composeWithFinalizer unreachable p1 p2

Conduit uses the same technique of defining `<+<` in terms of
a "compose with finalizer" function. Well to be honest, *I* stole the technique
from Conduit code, because I just couldn't figure out how to do it
on my own. However, after I got the idea from Conduit, I implemented it separately.
I knew that Conduit didn't use `unreachable`, but that doesn't really change
the behavior of the code. There is another important difference
that I will point out. Let's compare the code case by case.

> composeWithFinalizer :: Monad m => m ()
>                  -> Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> composeWithFinalizer finalizeUpstream p1 p2 = pipeM $ do
>   x1 <- runPipeM p1
>   let p1' = pipeM $ return x1
>   runPipeM $ pipeCase x1

    [haskell]
    pipe' final left right =
      case right of
        PipeM mp -> PipeM (liftM (pipe' final left) mp)

Note that one *unimportant* difference is that `pipe'` has the two pipe inputs
in the opposite order of `composeWithFinalizer`. So `left` is `p2` and `right`
is `p1`. We both begin by casing on the downstream pipe.

>   {- Return -} (\r       -> lift finalizeUpstream >> return r)

    [haskell]
        Done r2 -> PipeM (final >> return (Done r2))

If downstream returns, we both run the current finalizer and then
return the same result.

>   {- L-over -} (\_next l -> absurd l)

    [haskell]
        Leftover _ i -> absurd i

Obviously the same.

>   {- Yield  -} (\next finalizeDownstream o ->
>                        let (<*<) = composeWithFinalizer finalizeUpstream
>                        in wrap $ Yield
>                            (next <*< p2)
>                            (finalizeUpstream >> finalizeDownstream)
>                            o)

    [haskell]
        HaveOutput p c o -> HaveOutput (pipe' final left p) c o

Notice that `(next <*< p2)` is identical to `pipe' final left p`,
we both resuse the current finalizer for the `next` computation.
And we both yield the `o` without modification.
However, there is an important difference:
in the `yield` construct, I have created a new finalizer
by combining the "current" `finalizeUpstream` with the finalizer
found inside the `yield` we are inspecting.
This way, when control is transferred further downstream,
both `p1` and `p2` will have a chance to be finalized.
The conduit-0.5.2.2 implementation does *not* factor in the current upstream
finalizer (instead, it just passes `c` along),
and as I will later demonstrate, this causes undesirable behavior.
I have to admit, when I saw this discrepancy, I was unsure whether
I had missed something, or whether I was right. I put a lot of effort
into part 5 explaining finalization, and it turns out that
I was right, but not without a grave mistake of my own,
which I shall also demonstrate.

Let's press on with our comparison.

>   {- Await  -} (\f1 g1 -> pipeM $ do
>     x2 <- runPipeM p2
>     runPipeM $ pipeCase x2

    [haskell]
        NeedInput rp rc -> upstream rp rc
      where
        upstream rp rc =
          case left of
            PipeM mp -> PipeM (liftM (\left' -> pipe' final left' right) mp)

In the event of downstream `await`, control shifts upstream
in both implementations.

>     {- Return -} (\u'     -> g1 u' <+< return u')

    [haskell]
            Done r1 -> pipe (Done r1) (rc r1)

In the absence of `abort`, we must return to the broken record technique:
just keep giving the upstream result every time an upstream value is awaited.
This is identical to Conduit behavior.

>     {- L-over -} (\next l -> wrap $ Leftover (p1' <?< next) l)

    [haskell]
            Leftover left' i -> Leftover (pipe' final left' right) i

Here the only difference is that I use `unreachable` while
Conduit just passes the current finalizer. Since it will never be reached,
the behavior is the same.

>     {- Yield  -} (\next newFinalizer o ->
>                       let (<*<) = composeWithFinalizer newFinalizer
>                       in f1 o <*< next)

    [haskell]
            HaveOutput left' final' o -> pipe' final' left' (rp o)

When upstream yields to downstream, the choice is obvoius.
A new upstream finalizer is provided, so we both use that.

>     {- Await  -} (\f2 g2 -> wrap $ Await
>                           (\i -> p1' <?< f2 i)
>                           (\u -> p1' <?< g2 u)))

    [haskell]
            NeedInput left' lc -> NeedInput
              (\a -> pipe' final (left' a) right)
              (\r0 -> pipe' final (lc r0) right)

This is also the same, modulo `unreachable`.
Notice how in our code, we had to bind `p1'`,
the pipe we got *after* `runPipeM p1`. We wouldn't want
to re-invoke those effects all over again; they should only
be invoked once. The Conduit code doesn't have to worry about that,
since it partitions effects into `PipeM`.

> (>+>) :: Monad m => Pipe l i i' u m u' -> Pipe Void i' o u' m r -> Pipe l i o u m r
> (>+>) = flip (<+<)
> 
> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

It is easy to observe that `runPipe` is the same.

> runPipe :: Monad m => Pipeline m r -> m r
> runPipe p = do
>   e <- runPipeM p
>   pipeCase e
>   {- Return -} (\r             -> return r)
>   {- L-over -} (\_next l       -> absurd l)
>   {- Yield  -} (\_next _fin o  -> absurd o)
>   {- Await  -} (\f _g          -> runPipe $ f ())

    [haskell]
    runPipe :: Monad m => Pipe Void () Void () m r -> m r
    runPipe (PipeM mp) = mp >>= runPipe
    runPipe (Done r)              = return r
    runPipe (Leftover _ i)        = absurd i
    runPipe (HaveOutput _ _ o)    = absurd o
    runPipe (NeedInput _ c)       = runPipe (c ())

Getting rid of leftovers
-------------------------------------------------

The code is a little more involved here,
but inspect each case and you'll see that our implementations
of `injectLeftovers` are also identical.

> injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r
> injectLeftovers = go [] where
>   go ls p = pipeM $ do
>     x <- runPipeM p
>     runPipeM $ pipeCase x
>     {- Return -} (\r -> return r)
>     {- L-over -} (\next l -> go (l:ls) next)
>     {- Yield  -} (\next fin o -> wrap $ Yield (go ls next) fin o)
>     {- Await  -} (\f g -> case ls of
>       [] -> wrap $ Await (go [] . f) (go [] . g)
>       l : ls' -> go ls' (f l))

    [haskell]
    injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r
    injectLeftovers =
        go []
      where
        go ls (PipeM mp) = PipeM (liftM (go ls) mp)
        go _ (Done r) = Done r
        go ls (Leftover p l) = go (l:ls) p
        go ls (HaveOutput p c o) = HaveOutput (go ls p) c o
        go [] (NeedInput p c) = NeedInput (go [] . p) (go [] . c)
        go (l:ls) (NeedInput p _) = go ls $ p l

Adding finalizers to a pipe
-------------------------------------------------

`cleanupP` and `addCleanup` differ only in a matter of style:
Conduit's `addCleanup` finalizer takes a `Bool` input to determine
whether termination is "normal" or "abnormal", while
`cleanupP` takes two separate finalizers to cover the two cases.
The third `abort` case is obviously removed with the removal of `abort`.

> cleanupP :: Monad m => m () -> m () -> Pipe l i o u m r
>          -> Pipe l i o u m r
> cleanupP discardedFinalize returnFinalize = go where
>   go p = pipeM $ do
>     x <- runPipeM p
>     runPipeM $ pipeCase x

    [haskell]
    addCleanup :: Monad m => (Bool -> m ()) -> Pipe l i o u m r -> Pipe l i o u m r
    addCleanup cleanup (PipeM msrc) = PipeM (liftM (addCleanup cleanup) msrc)

Identical modulo pipeM/runPipeM.

>     {- Return -} (\r -> lift returnFinalize >> return r)

    [haskell]
    addCleanup cleanup (Done r) = PipeM (cleanup True >> return (Done r))

Here we see both invoke the "normal termination" finalizer.

>     {- L-over -} (\next l -> wrap $ Leftover (go next) l)

    [haskell]
    addCleanup cleanup (Leftover p i) = Leftover (addCleanup cleanup p) i

Identical.

>     {- Yield  -} (\next finalizeRest o -> wrap $
>                       Yield (go next) (finalizeRest >> discardedFinalize) o)

    [haskell]
    addCleanup cleanup (HaveOutput src close x) = HaveOutput
        (addCleanup cleanup src)
        (cleanup False >> close)
        x

Here we see both will pass along the "abnormal termination" finalizer.
However, we chose to order them differently. This may be significant.

>     {- Await  -} (\f g -> wrap $ Await (go . f) (go . g))

    [haskell]
    addCleanup cleanup (NeedInput p c) = NeedInput
        (addCleanup cleanup . p)
        (addCleanup cleanup . c)

Identical.

> finallyP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> finallyP finalize = cleanupP finalize finalize
> 
> catchP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> catchP finalize = cleanupP finalize pass
> 
> successP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> successP finalize = cleanupP pass finalize

I didn't see these combinators provided by Conduit,
but they are nothing more than trivial wrappers around `addCleanup`.
I patterned `bracketP` after the Conduit code so it should be no surprise
that they are identical modulo pipeM/runPipeM.
I think my code is a touch more readable, though I cannot speak for efficiency.

> bracketP :: MonadResource m => IO a -> (a -> IO ()) -> (a -> Pipe l i o u m r)
>          -> Pipe l i o u m r
> bracketP create destroy mkPipe = do
>   (key, val) <- lift $ allocate create destroy 
>   finallyP (release key) (mkPipe val)

    [haskell]
    bracketP alloc free inside =
        PipeM start
      where
        start = do
            (key, seed) <- allocate alloc free
            return $ addCleanup (const $ release key) (inside seed)

Finalization and associativity of composition
-------------------------------------------------

Let's explore the discrepancy in finalization.

> finallyC :: Monad m => m () -> C.Pipe l i o u m r -> C.Pipe l i o u m r
> finallyC fin = C.addCleanup (const fin)
> 
> idC :: Monad m => C.Pipe l i i u m u
> idC = C.awaitForever C.yield
> 
> printerC :: Show i => C.Pipe l i Void u IO u
> printerC = C.awaitForever $ lift . print
> 
> idMsgC :: String -> C.Pipe l i i u IO u
> idMsgC msg = finallyC (putStrLn msg) idC
> 
> takeC :: Monad m => Int -> C.Pipe l i i u m ()
> takeC 0 = return ()
> takeC n = C.awaitE >>= \ex -> case ex of
>   Left _u -> return ()
>   Right i -> C.yield i >> takeC (pred n)
> 
> testPipeC :: Show o => C.Pipe Void Int o () IO r -> IO r
> testPipeC p = C.runPipe $ printerC C.<+< p C.<+< C.sourceList [1..]

Now that we're equipped with a few convenient ways to create
pipes with finalizers, let's see what happens when we compose
three pipes together: the farthest downstream will cause termination,
and the two upstream of it will both contain finalizers.

    [ghci]
    testPipeC $ (takeC 2 C.<+< idMsgC "foo") C.<+< idMsgC "bar"
      1
      2
      foo
      bar
    testPipeC $ takeC 2 C.<+< (idMsgC "foo" C.<+< idMsgC "bar")
      1
      2
      foo

Where did the "bar" go? It is as I suspected, conduit-0.5.2.2 drops
the up-upstream finalizers. While I certainly approve of the use of
ResourceT, I'm afraid that relying on it too much could be hiding
these sorts of bugs in Conduit code.

The deeply scary thing about this is that it illustrates
that conduit composition is not associative. It's known now that pipes
with upstream results do not behave entirely like a Category,
but they nevertheless *should* try to behave as much like a Category
as possible, especially when you are constructing, composing, and running pipes
using only the primitives provided.

Let's take a look at my implementation and see how it handles
this situation.

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield
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
> printer :: Show i => Consumer l i u IO u
> printer = awaitForever $ lift . print
> 
> idMsg :: String -> Pipe l i i u IO u
> idMsg msg = finallyP (putStrLn msg) idP
> 
> take' :: Monad m => Int -> Pipe l i i u m ()
> take' 0 = return ()
> take' n = awaitE >>= \ex -> case ex of
>   Left _u -> return ()
>   Right i -> yield i >> take' (pred n)
> 
> testPipe :: Show o => Pipe Void Int o () IO r -> IO r
> testPipe p = runPipe $ printer <+< p <+< fromList [1..]

    [ghci]
    testPipe $ (take' 2 <+< idMsg "foo") <+< idMsg "bar"
      1
      2
      foo
      bar
    testPipe $ take' 2 <+< (idMsg "foo" <+< idMsg "bar")
      1
      2
      bar
      foo

Ugh! While it didn't drop the `bar` finalizer (yay!),
my choices for "consistency" were obviously wrong, because
it still does not preserve associativity of composition.

> printerF :: Show i => F.Frame Void IO (F.M i) F.C r
> printerF = foreverR $ (F.await !>= liftU . print)
> 
> idMsgF :: String -> F.Frame i IO (F.M i) F.C r
> idMsgF msg = F.finallyF (putStrLn msg) F.idF
> 
> takeF :: Int -> F.Frame i IO (F.M i) F.C ()
> takeF 0 = F.close
> takeF n = F.await !>= F.yield !> takeF (pred n)
> 
> fromListF :: [o] -> F.Frame o IO (F.M i) F.C ()
> fromListF xs = F.close !> mapMR_ F.yield xs
> 
> testPipeF :: Show o => F.Frame o IO (F.M Int) F.C () -> IO ()
> testPipeF p = F.runFrame $ printerF F.<-< p F.<-< fromListF [1..]

    [ghci]
    testPipeF $ (takeF 2 F.<-< idMsgF "foo") F.<-< idMsgF "bar"
      1
      2
      bar
      foo
    testPipeF $ takeF 2 F.<-< (idMsgF "foo" F.<-< idMsgF "bar")
      1
      2
      bar
      foo

Looks like somebody got it right. :)

Next time
-------------------------------------------------

There is no next time; that's it folks!
Personally, I will be taking a closer look at the order of finalizers;
hopefully we can pick an order that always preserves the associativity
of composition, and patch that into the next version of conduit!

There are still a lot of interesting options to explore
when it comes to implementing pipes. See also:

 * Paolo Capriotti's [pipes-core](https://github.com/pcapriotti/pipes-core)
 * Chris Smith's [my-pipes](https://github.com/cdsmith/my-pipes)
 * Gabriel Gonzalez's [pipes](https://github.com/Gabriel439/Haskell-Pipes-Library)
 * Michael Snoyman's [conduit](https://github.com/snoyberg/conduit)

You can play with this code for yourself by downloading
[PipeConduit.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeConduit.lhs).

