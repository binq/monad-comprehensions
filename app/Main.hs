module Main where

import Prelude hiding (getLine, putStr, putStrLn)
import Control.Concurrent.Async (async, waitCatch, cancel)
import Control.Concurrent.MVar (MVar ())
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), runReaderT)
import Text.Printf (printf)
import Data.String ()
import Data.Text (Text ())
import qualified Data.Text.IO as IO

newEmptyMVar :: forall a {m} . MonadIO m => m (MVar a)
newEmptyMVar = liftIO $ MVar.newEmptyMVar

putMVar :: MonadIO m => MVar a -> a -> m ()
putMVar m v = liftIO $ MVar.putMVar m v

readMVar :: MonadIO m => MVar a -> m a
readMVar p = liftIO $ MVar.readMVar p

getLine :: MonadIO m => m Text
getLine = liftIO $ IO.getLine

putStr :: MonadIO m => Text -> m ()
putStr v = liftIO $ IO.putStr v

putStrLn :: MonadIO m => Text -> m ()
putStrLn v = liftIO $ IO.putStrLn v

repl :: forall m c . (c ~ MVar (), MonadReader c m, MonadIO m) => m ()
repl = void $ putStr "% " >> getLine >>= process

process :: forall m c . (c ~ MVar (), MonadReader c m, MonadIO m) => Text -> m ()
process "quit" = [() | _ <- ask >>= (`putMVar` ()) >> newEmptyMVar >>= readMVar]
process l = liftIO $ printf "You said: %s!" l

main :: IO ()
main =
  do
    c <- newEmptyMVar @()
    w <- async . forever . (`runReaderT` c) $ repl
    readMVar c
    cancel w
    _ <- waitCatch w
    putStrLn "Goodbye!"
