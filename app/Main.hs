module Main where

import Prelude hiding (getLine, print, putStr, putStrLn, read)

import Control.Concurrent.Async (async, waitCatch, cancel)
import Control.Concurrent.MVar (MVar ())
import Control.Concurrent.MVar qualified as MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Text.Printf qualified as IO (printf)
import Data.Kind (Type)
import Data.String ()
import Data.Text (Text ())
import Data.Text.IO qualified as IO (getLine, putStr, putStrLn)

newEmptyMVar :: forall a {m} . MonadIO m => m (MVar a)
newEmptyMVar = liftIO $ MVar.newEmptyMVar

putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar m = liftIO . MVar.putMVar m

readMVar :: MonadIO m => MVar a -> m a
readMVar = liftIO . MVar.readMVar

getLine :: MonadIO m => m Text
getLine = liftIO $ IO.getLine

putStr :: MonadIO m => Text -> m ()
putStr = liftIO . IO.putStr

putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . IO.putStrLn

said :: MonadIO m => Text -> m ()
said = liftIO . IO.printf "You said: %s!\n"

type Incoming :: Type -> Type
data Incoming s where
  Got :: s -> Incoming s
  Terminate :: Incoming s

type Outgoing :: Type -> Type
data Outgoing s where
  Report :: s -> Outgoing s
  Pass :: Outgoing s

repl :: (c ~ MVar (), MonadReader c m, MonadIO m) => m ()
repl = forever [() | _ <- read >>= evaluate >>= print]

read :: MonadIO m => m (Incoming Text)
read = do
  putStr "% "
  getLine >>= \case
    "quit" -> pure Terminate
    v -> pure $ Got v

evaluate :: (c ~ MVar (), MonadReader c m, MonadIO m) => Incoming Text -> m (Outgoing Text)
evaluate =
  \case
    Terminate -> halt >> pure Pass
    Got v -> pure $ Report v

print :: MonadIO m => Outgoing Text -> m ()
print = 
  \case
    Pass -> pure ()
    Report v -> said v

halt :: (c ~ MVar (), MonadReader c m, MonadIO m) => m ()
halt = do
  c <- ask
  putMVar c ()
  _ <- newEmptyMVar >>= readMVar
  pure ()

main :: IO ()
main = do
  c <- newEmptyMVar @()
  w <- async $ runReaderT repl c
  readMVar c >> cancel w >> waitCatch w >> putStrLn "Goodbye!"
