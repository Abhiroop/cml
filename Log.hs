module Log (logMsg, logFlush) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import System.IO
import System.IO.Unsafe

{-# NOINLINE msgQ #-}
{-# OPTIONS_GHC -fno-cse #-}

msgQ :: MVar (Maybe (TChan (ThreadId,String)))
msgQ = unsafePerformIO (newMVar Nothing)

logMsg :: String -> IO ()
logMsg msg = do
  msgs <- maybeStart
  mytid <- myThreadId
  atomically $ writeTChan msgs (mytid, msg)

logFlush :: IO ()
logFlush = do
  mq <- readMVar msgQ
  case mq of
    Nothing -> return ()
    Just q  -> loop
      where
        loop = do
          e <- atomically $ isEmptyTChan q
          unless e $ do
            threadDelay 1000000
            loop

maybeStart :: IO (TChan (ThreadId, String))
maybeStart = modifyMVar msgQ maybeStart'
  where
    maybeStart' Nothing = do
      msgs <- atomically newTChan
      forkIO (logThread msgs)
      return (Just msgs,msgs)
    maybeStart' x@(Just msgs) = return (x,msgs)

    logThread msgs = forever $ do
      (tid,msg) <- atomically $ readTChan msgs
      hPutStrLn stderr $ "["++show tid++"] "++msg

test :: IO ()
test = logMsg "message" >> threadDelay 100000 >> logFlush
