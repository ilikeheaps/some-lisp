module MonadStuff where
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

readerT :: Monad m => Reader a b -> ReaderT a m b
readerT = mapReaderT (return . runIdentity)

catchMaybeT :: Monad t => MaybeT t a -> t a -> MaybeT t a
catchMaybeT mtry merr =
  lift $ do mval <- runMaybeT mtry
            maybe merr return mval
