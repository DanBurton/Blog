Last time, we implemented a bowling game scorer
by using a Tardis. If you aren't yet familiar with
the Tardis's interface, then I recommend you check out
[the explanation on Hackage](http://hackage.haskell.org/packages/archive/tardis/0.3.0.0/doc/html/Control-Monad-Tardis.html).
(tl;dr it's a State monad with get and put,
except there are two streams of state,
one forwards and one backwards,
so there are four operations:
`(++) <$> [get, send] <*> ["Past", "Future"]`).

Today, we'll take a large step in the esoteric drection,
and implement a Seer by using a Tardis.

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DoRec #-}

> import Control.Applicative (Applicative)
> import Control.Monad (liftM)
> import Control.Monad.Fix (MonadFix, mfix)
> import Control.Monad.Trans.Class (lift)
> import Control.Monad.Trans.Tardis
> import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
> import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)
> import Data.Monoid

What is a Seer?
======================================================================

> class (Monad m, Monoid w) => MonadSeer w m | m -> w where
>   see :: m w
>   send :: w -> m ()
>   contact :: w -> m w
>   
>   see = contact mempty
>   send w = contact w >> return ()
>   contact w = send w >> see

todo: prose

Seer in terms of a Tardis
======================================================================

> newtype SeerT w m a = SeerT { unSeerT :: TardisT w w m a }
>                     deriving (Functor, Applicative, Monad, MonadFix)

> instance (Monoid w, MonadFix m) => MonadSeer w (SeerT w m) where
>   contact w = SeerT $ do
>     rec past <- getPast
>         future <- getFuture
>         sendFuture (past <> w)
>         sendPast (w <> future)
>     return (past <> w <> future)

> runSeerT :: (MonadFix m, Monoid w) => SeerT w m a -> m a
> runSeerT = flip evalTardisT (mempty, mempty) . unSeerT

todo: prose
todo: prove it works

Seer in terms of a Reader/Writer
======================================================================

> newtype RWSeerT w m a = RWSeerT { unRWSeerT :: ReaderT w (WriterT w m) a }
>                       deriving (Functor, Applicative, Monad, MonadFix)

> instance (Monoid w, Monad m) => MonadSeer w (RWSeerT w m) where
>   see = RWSeerT ask
>   send w = RWSeerT (lift (tell w))

> runRWSeerT :: (Monoid w, MonadFix m) => RWSeerT w m a -> m a
> runRWSeerT (RWSeerT rwma) = liftM fst $
>   mfix (\ ~(_, w) -> runWriterT (runReaderT rwma w))

todo: prose
todo: prove it works

So why use a Tardis?
======================================================================

todo: prose

