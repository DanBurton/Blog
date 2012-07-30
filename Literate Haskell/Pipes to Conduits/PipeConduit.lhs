> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# OPTIONS_GHC -Wall #-}
> 
> module PipeConduit where
> 
> import Control.Monad.Trans.Free (FreeT(..), FreeF(..), liftF, wrap)
> 
> import Data.Void (Void, absurd)
> import Control.Applicative
> import Control.Monad (when)
> import Control.Monad.Trans.Class
> import Control.Monad.Trans.Resource (MonadResource, allocate, release)


Helpers
--------------------------------------------------

> pass :: Monad m => m ()
> pass = return ()
> 
> unreachable :: Monad m => m ()
> unreachable = error "You've reached the unreachable finalizer"


The Pipe type
--------------------------------------------------

> data PipeF l i o u m next
>   = Yield o (m ()) next
>   | Await (i -> next) (u -> next)
>   | Leftover l next
>   deriving Functor
> 
> newtype Pipe l i o u m r = Pipe
>   { unPipe :: FreeT (PipeF l i o u m) m r }
>   deriving (Functor, Applicative, Monad)
> 
> instance MonadTrans (Pipe l i o u) where
>   lift = Pipe . lift
> 
> type Producer   o   m r = Pipe Void () o    () m r
> type Consumer l i u m r = Pipe l    i  Void u  m r
> type Pipeline       m r = Pipe Void () Void () m r

> pipeM :: m (FreeF (PipeF l i o u m) r (FreeT (PipeF l i o u m) m r))
>       -> Pipe l i o u m r
> pipeM m = Pipe (FreeT m)
> 
> runPipeM :: Pipe l i o u m r
>          -> m (FreeF (PipeF l i o u m) r (FreeT (PipeF l i o u m) m r))
> runPipeM (Pipe (FreeT m)) = m

Working with PipeF
--------------------------------------------------

> pipeCase :: FreeF (PipeF l i o u m) r next
>  -> (r                          -> a) -- Return
>  -> (l -> next                  -> a) -- Leftover
>  -> (o -> m () -> next          -> a) -- Yield
>  -> ((i -> next) -> (u -> next) -> a) -- Await
>                                 -> a
> pipeCase (Return r)
>   k _ _ _ = k r
> pipeCase (Wrap (Leftover l next))
>   _ k _ _ = k l next
> pipeCase (Wrap (Yield o fin next))
>   _ _ k _ = k o fin next
> pipeCase (Wrap (Await f g))
>   _ _ _ k = k f g

> liftP :: Monad m => PipeF l i o u m a -> Pipe l i o u m a
> liftP = Pipe . liftF

> wrapP :: Monad m => PipeF l i o u m (FreeT (PipeF l i o u m) m a)
>       -> Pipe l i o u m a
> wrapP = Pipe . wrap

Pipe primitives
--------------------------------------------------

> awaitE :: Monad m =>        Pipe l i o u m (Either u i)
> awaitE = liftP $ Await Right Left
> 
> yield :: Monad m => o ->    Pipe l i o u m ()
> yield b = liftP $ Yield b pass ()
> 
> leftover :: Monad m => l -> Pipe l i o u m ()
> leftover l = liftP $ Leftover l ()

Pipe composition
--------------------------------------------------

> (<+<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <+< p2 = composeWithFinalizer pass p1 p2

> (<?<) :: Monad m => Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> p1 <?< p2 = composeWithFinalizer unreachable p1 p2

> composeWithFinalizer :: Monad m => m ()
>                  -> Pipe Void i' o u' m r -> Pipe l i i' u m u' -> Pipe l i o u m r
> composeWithFinalizer finalizeUpstream p1 p2 = pipeM $ do
>   let go fin down up = unPipe $ composeWithFinalizer fin (Pipe down) (Pipe up)
>   x1 <- runPipeM p1
>   let p1' = pipeM $ return x1
>   runPipeM $ pipeCase x1
>   {- Return -} (\r       -> lift finalizeUpstream >> return r)
>   {- L-over -} (\l _next -> absurd l)
>   {- Yield  -} (\o finalizeDownstream next ->
>                        let (<*<) = go finalizeUpstream
>                        in wrapP $ Yield o
>                            (finalizeUpstream >> finalizeDownstream)
>                            (next <*< unPipe p2))
>   {- Await  -} (\f1 g1 -> pipeM $ do
>     x2 <- runPipeM p2
>     runPipeM $ pipeCase x2
>     {- Return -} (\u'     -> Pipe (g1 u') <+< return u')
>     {- L-over -} (\l next -> wrapP $ Leftover l (unPipe $ p1' <?< Pipe next))
>     {- Yield  -} (\o newFinalizer next ->
>                       let (<*<) = go newFinalizer
>                       in Pipe $ f1 o <*< next)
>     {- Await  -} (\f2 g2 -> wrapP $ Await
>                           (\i -> unPipe $ p1' <?< Pipe (f2 i))
>                           (\u -> unPipe $ p1' <?< Pipe (g2 u))))

> (>+>) :: Monad m => Pipe l i i' u m u' -> Pipe Void i' o u' m r -> Pipe l i o u m r
> (>+>) = flip (<+<)
> 
> infixr 9 <+<
> infixr 9 >+>


Running a pipeline
--------------------------------------------------

> runPipe :: Monad m => Pipeline m r -> m r
> runPipe p = do
>   e <- runPipeM p
>   pipeCase e
>   {- Return -} (\r             -> return r)
>   {- L-over -} (\l _next       -> absurd l)
>   {- Yield  -} (\o _fin _next  -> absurd o)
>   {- Await  -} (\f _g          -> runPipe $ Pipe $ f ())


Getting rid of leftovers
-------------------------------------------------

> injectLeftovers :: Monad m => Pipe i i o u m r -> Pipe l i o u m r
> injectLeftovers = go [] where
>   go ls p = pipeM $ do
>     x <- runPipeM p
>     runPipeM $ pipeCase x
>     {- Return -} (\r -> return r)
>     {- L-over -} (\l next -> go (l:ls) (Pipe next))
>     {- Yield  -} (\o fin next -> wrapP $ Yield o fin (unPipe $ go ls (Pipe next)))
>     {- Await  -} (\f g -> case ls of
>       [] -> wrapP $ Await (unPipe . go [] . Pipe . f) (unPipe . go [] . Pipe . g)
>       l : ls' -> go ls' (Pipe $ f l))

Adding finalizers to a pipe
-------------------------------------------------

> cleanupP :: Monad m => m () -> m () -> Pipe l i o u m r
>          -> Pipe l i o u m r
> cleanupP discardedFinalize returnFinalize = go where
>   go p = pipeM $ do
>     x <- runPipeM p
>     runPipeM $ pipeCase x
>     {- Return -} (\r -> lift returnFinalize >> return r)
>     {- L-over -} (\l next -> wrapP $ Leftover l (unPipe $ go $ Pipe next))
>     {- Yield  -} (\o finalizeRest next -> wrapP $
>                       Yield o (finalizeRest >> discardedFinalize) (unPipe $ go $ Pipe next))
>     {- Await  -} (\f g -> wrapP $
>                       Await (unPipe . go . Pipe . f) (unPipe . go . Pipe . g))

> finallyP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> finallyP finalize = cleanupP finalize finalize
> 
> catchP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> catchP finalize = cleanupP finalize pass
> 
> successP :: Monad m => m () -> Pipe l i o u m r -> Pipe l i o u m r
> successP finalize = cleanupP pass finalize

> bracketP :: MonadResource m => IO a -> (a -> IO ()) -> (a -> Pipe l i o u m r)
>          -> Pipe l i o u m r
> bracketP create destroy mkPipe = do
>   (key, val) <- lift $ allocate create destroy 
>   finallyP (release key) (mkPipe val)


Some basic pipes
-------------------------------------------------

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

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



Next time
-------------------------------------------------


You can play with this code for yourself by downloading
[PipeClose.lhs](https://raw.github.com/DanBurton/Blog/master/Literate%20Haskell/Pipes%20to%20Conduits/PipeClose.lhs).

