module Main where

import Network hiding (accept, sClose)
import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent {- hiding (forkFinally) instead using myFOrkFinally to avoid GHC version issues-}
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, join)
import Data.List.Split
import qualified Data.Map as M hiding (split)
import Prelude hiding (null, lookup)
import Text.Printf (printf)


import ChatServer

maxThreadCount :: Int
maxThreadCount = 16

main:: IO ()
main = withSocketsDo $ do

    server <- newChatServer serverhost serverport
    --sock <- listenOn (PortNumber (fromIntegral serverport))

    addrinfos <- getAddrInfo
			 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
			 Nothing (Just serverport)

    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 5

    _ <- printf "Listening on port %s\n" serverport

    threadCount <- atomically $ newTVar 0
	--New Abstract FIFO Channel
    chan <- newChan
	--Spawns a new thread to handle the clientconnectHandler method, passes socket, channel, numThreads and server
    forkIO $ clientconnectHandler sock chan numThreads server
  
    --Calls the mainHandler which will monitor the FIFO channel
    mainHandler sock chan

mainHandler :: Socket -> Chan String -> IO ()
mainHandler sock chan = do

  --Read current message on the FIFO channel
  chanMsg <- readChan chan

  --If KILL_SERVICE, stop mainHandler running, If anything else, call mainHandler again, keeping the service running
  case (chanMsg) of
    ("KILL_SERVICE") -> putStrLn "Terminating the Service!"
    _ -> mainHandler sock chan

clientconnectHandler :: Socket -> Chan String -> TVar Int -> Server -> IO ()
clientconnectHandler sock chan numThreads server = do

  --Accept the socket which returns a handle, host and port
  --(handle, host, port) <- accept sock
  (s,a) <- accept sock
  handle <- socketToHandle s ReadWriteMode
  --Read numThreads from memory and print it on server console
  count <- atomically $ readTVar numThreads
  putStrLn $ "numThreads = " ++ show count

  --If there are still threads remaining create new thread and increment (exception if thread is lost -> decrement), else tell user capacity has been reached
  if (count < maxnumThreads) then do
    forkFinally (clientHandler s chan server) (\_ -> atomically $ decrementTVar numThreads)
    atomically $ incrementTVar numThreads
    else do
      hPutStrLn handle "Maximum number of threads in use. try again soon"
      hClose handle

  clientconnectHandler sock chan numThreads server

    --forever $ do
    --    count <- atomically $ readTVar threadCount
     --   putStrLn $ "threadCount = " ++ show count
    --
    --    (clientSock, connAddr) <- accept sock
    --    _ <- printf "Accepted connection from %s\n" (show connAddr)

--        if (count < maxThreadCount) then do
  --          forkFinally (clientHandler clientSock server) (\_ -> atomically $ decrementTVar threadCount)
    --        atomically $ incrementTVar threadCount
      --      else do
        --        send clientSock "Service reached maximum capacity, please try again later!"
          --      sClose clientSock

serverport :: String
serverport = "7007"

serverhost :: String
serverhost = "192.168.1.112"

incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)