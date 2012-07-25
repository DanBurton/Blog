Last time, we reimplemented Control.Pipe,
with basic `await` and `yield` functionality.
However, in order to compose two pipes,
their result types had to be the same,
and whenever any pipe in a pipeline reached its return,
it would bring down all the other pipes composed with it.

This time, we'll modify the `await` primitive,
forcing the user to deal with the possibility
that the upstream pipe has completed and returned a value.

> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeU where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> import Fun ((:&:)(..), (:|:)(..))
> 
> import Data.Void (Void)
> import Control.Monad (when)
> import Control.Monad.Trans.Class (lift)


Functors
--------------------------------------------------

We'll use all the same functors as before.

You can compare this code with the code from last time
to see exactly which changes have taken place.

We'll add one more convention, which is to use
the type variable `u` to describe the return type
of an upstream pipe.

> newtype Then next = Then next            -- Identity
> newtype Yield o next = Yield o           -- Const
> newtype Await i next = Await (i -> next) -- Fun

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

We're going to modify `Await` so that it also
considers the possibility of a completed upstream pipe.
That means that anytime you `await`, you could get
an `i` *or* a `u`, and you need to be prepared
to handle both situations.

> type YieldThen o = Yield o :&: Then
> type AwaitU i u = Await i :&: Await u

Because `AwaitU` demands a new type variable `u`,
so must `PipeF` and `Pipe`.

> type PipeF i o u = YieldThen o :|: AwaitU i u
> type Pipe i o u  = FreeT (PipeF i o u)

We'll add `u` to the Consumer type,
because consumers must now be aware of the
upstream result of the pipe they are composed with.
We'll use the trivial upstream result `()` for Producers
and Pipelines, since they will never get one anyways.

> type Producer o   = Pipe () o    ()
> type Consumer i u = Pipe i  Void u
> type Pipeline     = Pipe () Void ()

Remember: Consumers are always as far *downstream* as possible,
while Producers are always as far *upstream* as possible.
A Pipeline is neither up nor down, since it is self-contained
and therefore cannot be sensibly composed with any other pipes.


Working with PipeF
--------------------------------------------------

Our "lifting" helpers remain the same,
except we must add the type variable `u`
everywhere we have an `AwaitU`, `PipeF`, or `Pipe`.
`awaitF` now has two inputs: the function to deal with
a regular yielded value `f :: i -> next`,
and the function to deal with
a returned result `g :: u -> next`.

> liftYield :: YieldThen o next ->        PipeF i o u next
> liftYield = L
> 
> liftAwait :: AwaitU i u next ->         PipeF i o u next
> liftAwait = R
> 
> yieldF :: o -> next ->                  PipeF i o u next
> yieldF o next = liftYield $ Yield o :&: Then next
> 
> awaitF :: (i -> next) -> (u -> next) -> PipeF i o u next
> awaitF f g = liftAwait $ Await f :&: Await g

We also update `pipeCase` to reflect the new function
that is bundled with an `AwaitU`.

> pipeCase :: FreeF (PipeF i o u) r next
>          -> (r                          -> a) -- Return
>          -> (o -> next                  -> a) -- Yield
>          -> ((i -> next) -> (u -> next) -> a) -- Await
>                                         -> a
> pipeCase (Return r) k _ _ = k r
> pipeCase (Wrap (L (Yield o :&: Then next)))
>                     _ k _ = k o next
> pipeCase (Wrap (R (Await f :&: Await g)))
>                     _ _ k = k f g


Pipe primitives
--------------------------------------------------

We can no longer write `await`, because we
have to deal with the possibility of a returned result.
So we'll have to settle with `awaitE`, which provides
`Either` a `u` or an `i`.

> awaitE :: Monad m => Pipe i o u m (Either u i)
> awaitE  = liftF $ awaitF Right Left

Notice, where we used to have `id`, we now have `Right` and `Left`.
We need a `next` of type `Either u i`, so the first argument
to awaitF must be `i -> Either u i`, and the second
must be `u -> Either u i`. The types make the choice obvious.

Yield remains the same:

> yield :: Monad m => o -> Pipe i o u m ()
> yield b = liftF $ yieldF b ()


Pipe composition
--------------------------------------------------

Time to tweak the way pipes are composed.
We no longer want the upstream pipe to hijack the downstream one
when it has a result. Let's dive in and see how it pans out

> (<+<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r

Notice how composition not only moves input/output pairs

    (i => i') >+> (i' => o) = (i => o)

but also result types

    (u => u') >+> (u' => r) = (u => r)

> p1 <+< p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1
>   {- Return -} (\r      -> return r)
>   {- Yield  -} (\o next -> wrap $ yieldF o (next <+< p2))

Up until this point, everything is the same.

>   {- Await  -} (\f1 g1  -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2

The await case now has two functions at its disposal,
the new one `g1` for... well... I feel like I'm beating a dead horse
explining this every time it comes up.

Now, here was my first impulse for handling the upstream return:

    [haskell]
    {- Return -} (\u' -> g1 u')

Simple, right? If you have an upstream result, then guess what,
we have a function that is waiting for that input.
But here's the problem. g1 came from `p1 :: Pipe i' o u' m r`.
That means that it has the type `u' -> Pipe i o u' m r`,
and therefore, when we apply a `u'`, we get a `Pipe i o u' m r`.
Well that's a problem, see, because our result type is supposed to be
`Pipe i o u m r`: that's `u` not `u'`. So what do we do?
Well, we could compose it with an exploding bomb to get the correct type:

    [haskell]
    {- Return -} (\u' -> g1 u' <+< error "kaboom!")

That's not very nice. We could write in the docs that you should never
await after you've gotten an upstream result, but that's just gross.

How about something more sensible: just compose it with a pipe
that will return the same upstream result all over again,
in case you forgot what it was.

>     {- Return -} (\u'     -> g1 u' <+< return u')

Now we can safely say in the docs that once you get an upstream result,
you will just keep getting that same result every time you await.
That seems a lot less evil.

>     {- Yield  -} (\o next -> f1 o <+< next)
>     {- Await  -} (\f2 g2  -> wrap $ awaitF (\i -> p1' <+< f2 i)
>                                            (\u -> p1' <+< g2 u)))

Yield looks the same, and to handle an upstream await,
we just extend what we had before by mirroring the treatment of `f2`
to extend to `g2` in like manner.

Well there, we made it through again!
Although once again, we get sort of a sour taste
from the implementation we were forced to write.
If only we had a way to deal even *more* explicitly with
pipe termination... hrm... I'm feeling another blog post
coming on...

> (>+>) :: Monad m => Pipe i i' u m u' -> Pipe i' o u' m r -> Pipe i o u m r
> (>+>) = flip (<+<)

> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

The extra function `g` has no significance
when "running" a pipeline, so we will just ignore it
and retain essentially the same `runPipe` as before:

> runPipe :: Monad m => Pipeline m r -> m r
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Return -} (\r       -> return r)
>   {- Yield  -} (\_o next -> runPipe next)
>   {- Await  -} (\f _g    -> runPipe $ f ())


Some basic pipes
-------------------------------------------------

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

Since we no longer have the same `await`,
we'll have to rethink the way that we write pipe code.
We often used the idiom `forever $ await >>= foo`,
and it turns out that we can still simulate something like that:

> awaitForever :: Monad m => (i -> Pipe i o u m r) -> Pipe i o u m u
> awaitForever f = go where
>   go = awaitE >>= \ex -> case ex of
>     Left u  -> return u
>     Right i -> f i >> go

Conduit users may recognize `awaitForever`: it loops and loops
until the upstream pipe returns a result, and then it just
passes that upstream result right along, therefore
it has the same result type as upstream.

We can write many of the functions we had before,
but they will now bear the restriction of returning
the upstream result type.

> pipe :: Monad m => (i -> o) -> Pipe i o u m u
> pipe f = awaitForever $ yield . f

> idP :: Monad m => Pipe i i u m u
> idP = pipe id

> filterP :: Monad m => (i -> Bool) -> Pipe i i u m u
> filterP test = awaitForever $ \x -> when (test x) (yield x)

> printer :: Show i => Consumer i u IO u
> printer = awaitForever $ lift . print

Due to this limitation, perhaps we want to provide a different result
for a particular pipe.

> mapResult :: Monad m => (r -> r') -> Pipe i o u m r -> Pipe i o u m r'
> mapResult f p = do
>   r <- p
>   return (f r)
> 
> overwriteResult :: Monad m => r' -> Pipe i o u m r -> Pipe i o u m r'
> overwriteResult r p = p >> return r

My mental hlint is going BEEP BEEP BEEP right now,
because `mapOutput` is just `fmap`,
and `overwriteResult` is just `fmap . const`.

Newfound power
-------------------------------------------------

Now that our pipe composition works better with result types,
we can write combinators that have nontrivial result types!

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

Play around in ghci and see for yourself:

    [ghci]
    runPipe $ runP <+< (fromList [1 .. 10] >> return "foo")
    runPipe $ fold (+) 0 <+< fromList [10, 20, 100]
    runPipe $ (printer >> return "not hijacked") <+< return "hijacked"

Next time
-------------------------------------------------

The new powers that our enhanced pipe composition give us are nice,
but we had to give up the ability to *not* care, and instead
we have to write code in a slightly different style.
What's more, upstream pipes now have *no* power over their downstream
counterparts; once an upstream pipe returns, it is forever doomed
to just keep returning that same result, which is sort of weird.

Next time, we'll explore a new primitive, `abort`, and
restore the ability for any pipe to abort the entire pipeline.

