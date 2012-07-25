> {-# LANGUAGE TypeOperators #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeU where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> import Fun ((:&:)(..), (:|:)(..))
> 
> import Data.Void (Void)
> import Control.Monad (when, forever)
> import Control.Monad.Trans.Class (lift)


Functors
--------------------------------------------------

> newtype Then next = Then next            -- Identity
> newtype Yield o next = Yield o           -- Const
> newtype Await i next = Await (i -> next) -- Fun
> data Abort next = Abort               -- Empty

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

> type PipeF i o u = YieldThen o :|: AwaitU i u :|: Abort
> type Pipe i o u  = FreeT (PipeF i o u)

> type Producer o   = Pipe () o    ()
> type Consumer i u = Pipe i  Void u
> type Pipeline     = Pipe () Void ()


Working with PipeF
--------------------------------------------------

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

> yield :: Monad m => o -> Pipe i o u m ()
> yield b = liftF $ yieldF b ()

> abort :: Monad m => Pipe i o u m r
> abort = liftF abortF

Pipe composition
--------------------------------------------------

> (<+<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> p1 <+< p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1
>   {- Abort  -} (abort)               -- upstream discarded
>   {- Return -} (\r      -> return r) -- upstream discarded
>   {- Yield  -} (\o next -> wrap $ yieldF o (next <+< p2))
>   {- Await  -} (\f1 g1  -> FreeT $ do
>     x2 <- runFreeT p2
>     runFreeT $ pipeCase x2
>     {- Abort  -} (abort)             -- downstream discarded
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

> awaitForever :: Monad m => (i -> Pipe i o u m r) -> Pipe i o u m u
> awaitForever f = go where
>   go = awaitE >>= \ex -> case ex of
>     Left u  -> return u
>     Right i -> f i >> go

> await :: Monad m => Pipe i o u m i
> await = awaitE >>= \ex -> case ex of
>   Left _u -> abort
>   Right i -> return i


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

    [ghci]
    runPipe $ (printer >> return "not hijacked") <+< return "hijacked"
    runPipe $ (oldPrinter >> return "not hijacked") <+< return "hijacked"

Next time
-------------------------------------------------


