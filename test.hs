module Main where

import System.Environment (getArgs)

import Control.Concurrent.CML

import Log

main :: IO ()
main = do
  args <- getArgs
  let n = case args of (arg:_) -> read arg; _ -> 1
  mapM_ test ([1..n] :: [Integer])
  getChar >> return ()

test :: Integer -> IO ()
test n = do
  x <- channel
  y <- channel
  z <- channel

  let abortTx = logMsgTest n "Aborted transmit on x"
      abortRx = logMsgTest n "Aborted receive on x"
      abortTy = logMsgTest n "Aborted transmit on y"
      abortRy = logMsgTest n "Aborted receive on y"
      abortTz = logMsgTest n "Aborted transmit on z"
      abortRz = logMsgTest n "Aborted receive on z"
      guardTx = logMsgTest n "Trying transmit on x"
      guardRx = logMsgTest n "Trying receive on x"
      guardTy = logMsgTest n "Trying transmit on y"
      guardRy = logMsgTest n "Trying receive on y"
      guardTz = logMsgTest n "Trying transmit on z"
      guardRz = logMsgTest n "Trying receive on z"

  let wrapTx _ = logMsgTest n "Done transmit on x"
      wrapRx _ = logMsgTest n "Done receive on x"
      wrapTy _ = logMsgTest n "Done transmit on y"
      wrapRy _ = logMsgTest n "Done receive on y"
      wrapTz _ = logMsgTest n "Done transmit on z"
      wrapRz _ = logMsgTest n "Done receive on z"

  spawn $ sync $ choose
    [ guard (guardTx >> return (wrapabort abortTx (wrap (transmit x "Mx") wrapTx)))
    , guard (guardTy >> return (wrapabort abortTy (wrap (transmit y "My") wrapTy)))
    ]

  spawn $ sync $ choose
    [ guard (guardRy >> return (wrapabort abortRy (wrap (receive y (const True)) wrapRy)))
    , guard (guardRx >> return (wrapabort abortRx (wrap (receive x (const True)) wrapRx)))
    ]

  spawn $ sync $ guard
    (guardRz >> return (wrapabort abortRz (wrap (receive z (const True)) wrapRz)))

  sync $ guard
    (guardTz >> return (wrapabort abortTz (wrap (transmit z "Mz") wrapTz)))

logMsgTest :: Integer -> String -> IO ()
logMsgTest n s = logMsg $ "Test " ++ show n ++ ": " ++ s
