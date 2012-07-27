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
> 
> pass :: Monad m => m ()
> pass = return ()

The Pipe type
--------------------------------------------------

> type YieldThen o m = Yield o :&: Finalize m :&: Then
> type AwaitU i u    = Await i :&: Await u :&: Then

> type PipeF i o u m = YieldThen o m :|: AwaitU i u :|: Abort
> type Pipe i o u m r = FreeT (PipeF i o u m) m r
> 
> type Producer o   m r = Pipe () o    () m r
> type Consumer i u m r = Pipe i  Void u  m r
> type Pipeline     m r = Pipe () Void () m r


Working with PipeF
--------------------------------------------------

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
> yieldF o m next = liftYield $ Yield o :&: Finalize m :&: Then next
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
> pipeCase (Wrap (L (L (Yield o :&: Finalize m :&: Then next))))
>   _ _ k _ = k o m next
> pipeCase (Wrap (L (R (Await f :&: Await g :&: Then next))))
>   _ _ _ k = k f g next


Pipe primitives
--------------------------------------------------

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

> (<+<) :: Monad m => Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> p1 <+< p2 = composeWithFinalizer pass p1 p2

> composeWithFinalizer :: Monad m => m ()
>                  -> Pipe i' o u' m r -> Pipe i i' u m u' -> Pipe i o u m r
> composeWithFinalizer finalizeUpstream p1 p2 = FreeT $ do
>   x1 <- runFreeT p1
>   let p1' = FreeT $ return x1
>   runFreeT $ pipeCase x1
>   {- Abort  -} (      lift finalizeUpstream >> abort)
>   {- Return -} (\r -> lift finalizeUpstream >> return r)
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
>     {- Yield  -} (\o newFinalizer next ->
>                       let (<*<) = composeWithFinalizer newFinalizer
>                       in f1 o <*< next)
>     {- Await  -} (\f2 g2 onAbort2 ->
>                       let (<*<) = composeWithFinalizer (error msg)
>                           msg = "You reached the impossible finalizer"
>                       in wrap $ awaitF
>                           (\i -> p1' <*< f2 i)
>                           (\u -> p1' <*< g2 u)
>                           (      p1' <*< onAbort2)))


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
>   {- Abort  -} (                  return Nothing)
>   {- Return -} (\r             -> return $ Just r)
>   {- Yield  -} (\_o _fin next  -> runPipe next)
>   {- Await  -} (\f _g _onAbort -> runPipe $ f ())


Adding finalizers to a pipe
-------------------------------------------------

> cleanupP :: Monad m => m () -> m () -> Pipe i o u m r -> Pipe i o u m r
> cleanupP abortFinalize returnFinalize = go where
>   go p = FreeT $ do
>     x <- runFreeT p
>     runFreeT $ pipeCase x
>     {- Abort  -} (      lift abortFinalize  >> abort)
>     {- Return -} (\r -> lift returnFinalize >> return r)
>     {- Yield  -} (\o finalizeRest next -> wrap $
>                         yieldF o (finalizeRest >> abortFinalize) (go next))
>     {- Await  -} (\f g onAbort -> wrap $
>                         awaitF (go . f) (go . g) (go onAbort))

> finallyP :: Monad m => m () -> Pipe i o u m r -> Pipe i o u m r
> finallyP finalize = cleanupP finalize finalize
> 
> catchP :: Monad m => m () -> Pipe i o u m r -> Pipe i o u m r
> catchP finalize = cleanupP finalize pass
> 
> successP :: Monad m => m () -> Pipe i o u m r -> Pipe i o u m r
> successP finalize = cleanupP pass finalize

> bracketP :: MonadResource m => IO a -> (a -> IO ()) -> (a -> Pipe i o u m r)
>          -> Pipe i o u m r
> bracketP create destroy mkPipe = do
>   (key, val) <- lift $ allocate create destroy 
>   finallyP (release key) (mkPipe val)

> idMsg :: String -> Pipe i i u IO u
> idMsg str = finallyP (putStrLn str) idP

> testPipeR :: Monad m => Pipe i o u m r -> m (Maybe r)
> testPipeR p = runPipe $ (await >> abort) <+< p <+< abort

> testPipeL :: Monad m => Pipe Int o () m r -> m (Maybe r)
> testPipeL p = runPipe $ (await >> await >> abort) <+< take' 1 <+< p <+< fromList [1 ..]

> testPipe :: Monad m => Pipe Int o () m r -> m (Maybe r)
> testPipe p = runPipe $ evalP <+< p <+< fromList [1..]

> take' :: Monad m => Int -> Pipe i i u m ()
> take' 0 = pass
> take' n = (await >>= yield) >> take' (pred n)

Some basic pipes
-------------------------------------------------

> fromList :: Monad m => [o] -> Producer o m ()
> fromList = mapM_ yield

> awaitE :: Monad m => Pipe i o u m (Either u i)
> awaitE = tryAwait >>= \emx -> case emx of
>   Left Nothing  -> abort
>   Left (Just u) -> return $ Left u
>   Right i       -> return $ Right i

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

> await :: Monad m => Pipe i o u m i
> await = awaitE >>= \ex -> case ex of
>   Left _u -> abort
>   Right i -> return i

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

    [ghci]
    runPipe $ (printer >> return "not hijacked") <+< return "hijacked"
      Just "not hijacked"
    runPipe $ (oldPrinter >> return "not hijacked") <+< return "hijacked"
      Nothing


Next time
-------------------------------------------------


You can play with this code for yourself by downloading
[PipeRecover.lhs]().
