Sort of in response to
[Breaking from a Loop](http://www.haskellforall.com/2012/07/breaking-from-loop.html).

Sometimes I wish Haskell culture embraced `ContT` more.

> import Control.Monad.Cont
> import Control.Monad.State
> import Control.Monad.IO.Class

**loopForever**

Suppose I wanted to break out of loops
using a user-defined label for "break".
First, assume I am working with
some data type `LoopT`:

    [haskell]
    data LoopT m a = ...
    runLoopT :: Monad m => LoopT m a -> m a

And suppose I wanted to write something
like the following:

> getNonNullLine :: IO String
> getNonNullLine = runLoopT $ loopForever $ \break -> do
>   str <- liftIO getLine
>   when (not $ null str) $ break str

What type would `loopForever` have to have
in order to make this work?

    loopForever :: Monad m
                => (  (a -> LoopT m ())  -- label
                   -> LoopT m b          -- loop body
                   )         
                -> LoopT m a             -- type of whole expression

Hrm... now how are we going to implement
`LoopT`, `runLoopT`, and `loopForever`?
Well gee, the type of `loopForever` sure looks familiar...
in fact, it's nearly identical to callCC!

    callCC :: MonadCont m => ((a -> m b) -> m b) -> m a

It turns out that implementing it in terms of `callCC`
and `forever` is trivial:

> loopForever :: MonadCont m => ((a -> m c) -> m b) -> m a
> loopForever f = callCC (forever . f)

> runLoopT :: Monad m => ContT a m a -> m a
> runLoopT = flip runContT (\a -> return a)

The reasoning is straightforward:
I want to `callCC` on `f`, but first,
I want to apply "forever" to the "body" of `f`,
hence `callCC (forever . f)`.

Another silly example using State:

> untilS :: MonadState s m => (s -> Bool) -> (s -> s) -> m s
> untilS test inc = runLoopT $ loopForever $ \break -> do
>   s <- get
>   when (test s) $ break s
>   put $! inc s

Testing:

    [ghci]
    flip runState 3 $ untilS (==5) (+1)

**Foreach**

Breaking out of 'foreach' loops can be written just as easily:

> foreachExample :: IO ()
> foreachExample = runLoopT_ $ do
>   foreach_ [1 .. 3] $ \breakI i -> do
>     foreach_ [4 .. 6] $ \breakJ j -> do
>       when (j == 6) $ breakJ ()
>       liftIO $ print (i, j)

> foreach_ :: MonadCont m => [i] -> ((() -> m c) -> i -> m b) -> m ()
> foreach_ is f = callCC (forM_ is . f)

> runLoopT_ :: Monad m => ContT () m a -> m ()
> runLoopT_ = flip runContT (\_ -> return ())

Notice how similar `foreach_` is to `loopForever`.
We just modify the body of `f` in a different way,
this time applying `forM_ is`.
The plumbing is slightly different,
since foreach_ loops are used exclusively
for their side effects.
Writing a version of `foreach` that uses `forM`
instead of `forM_`
is left as an exercise for the reader.

Testing:

    [ghci]
    foreachExample

There are obviously different tradeoffs to using ContT,
and I certainly do endorse using EitherT and MaybeT for such things.
I just find it lamentable that ContT is treated like the ugly duckling.
`ContT` is not to be feared.

To play with this code, download
[loop.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/loop.lhs).
