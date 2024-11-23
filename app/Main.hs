module Main where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (forever)
import Text.Printf (printf)

import Control.Concurrent.Async (async, waitCatch, cancel)

main :: IO ()
main = do
  c <- newEmptyMVar @()
  w <- async . forever $ putStr "% " >> getLine >>= \case
      "quit" -> [() | _ <- putMVar c () >> newEmptyMVar >>= readMVar]
      l -> printf "You said: %s!\n" l
  [() | _ <- readMVar c >> cancel w >> waitCatch w >> putStrLn "Goodbye!"]
