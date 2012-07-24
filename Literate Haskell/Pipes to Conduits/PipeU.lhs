> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}

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

> type YieldThen o = Yield o :&: Then
> type AwaitU i u = Await i :&: Await u

> type PipeF i o u = YieldThen o :|: AwaitU i u
> type Pipe i o u  = FreeT (PipeF i o u)

> type Producer o   = Pipe () o    ()
> type Consumer i u = Pipe i  Void u
> type Pipeline     = Pipe () Void ()


Working with PipeF
--------------------------------------------------

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

> awaitE :: Monad m => Pipe i o u m (Either u i)
> awaitE  = liftF $ awaitF Right Left

> yield :: Monad m => o -> Pipe i o u m ()
> yield b = liftF $ yieldF b ()


Pipe composition
--------------------------------------------------

> (<+<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> p1 <+< p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1
>   {- Return -} (\r      -> return r)
>   {- Yield  -} (\o next -> wrap $ yieldF o (next <+< p2))
>   {- Await  -} (\f1 g1  -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2
>     {- Return -} (\u'     -> g1 u' <+< return u')
>     {- Yield  -} (\o next -> f1 o <+< next)
>     {- Await  -} (\f2 g2  -> wrap $ awaitF (\i -> p1' <+< f2 i)
>                                            (\u -> p1' <+< g2 u)))


> (>+>) :: Monad m => Pipe i i' u m u' -> Pipe i' o u' m r -> Pipe i o u m r
> (>+>) = flip (<+<)

> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

> runPipe :: Monad m => Pipeline m r -> m r
> runPipe p = do
>   e <- runFreeT p
>   pipeCase e
>   {- Return -} (\r       -> return r)
>   {- Yield  -} (\_o next -> runPipe next)
>   {- Await  -} (\f _g    -> runPipe $ f ())


Some basic pipes
-------------------------------------------------

Here's just a few pipes to play with.
Fire up ghci and make sure they work as expected.

> awaitForever :: Monad m => (i -> Pipe i o u m r) -> Pipe i o u m u
> awaitForever f = go where
>   go = awaitE >>= \ex -> case ex of
>     Left u  -> return u
>     Right i -> f i >> go

> pipe :: Monad m => (i -> o) -> Pipe i o u m u
> pipe f = awaitForever $ yield . f

> idP :: Monad m => Pipe i i u m u
> idP = pipe id

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

> filterP :: Monad m => (i -> Bool) -> Pipe i i u m u
> filterP test = awaitForever $ \x -> when (test x) (yield x)

> printer :: Show i => Consumer i u IO u
> printer = awaitForever $ lift . print

Next time
-------------------------------------------------


