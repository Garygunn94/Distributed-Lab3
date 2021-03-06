{-#LANGUAGE RecordWildCards #-}

module ChatServer where

import Network hiding (accept, sClose)
import Network.Socket hiding (send, recv, sendTo, recvFrom, Broadcast)
import Network.Socket.ByteString
import Data.ByteString.Char8 (pack, unpack)
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, join, mapM, mapM_, forM, forM_, sequence, sequence_)
import Data.List.Split
import qualified Data.Map as M hiding (split)
import Prelude hiding (null, lookup)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Set as S
import qualified Data.Map as M
{-
    Types
-}
type ClientName = String
type ChatroomName = String
type ChatroomRef = Integer
type ClientJoinID = Integer
{-
    ChatServer
-}
data ChatServer = ChatServer
    { address         :: String
    , port            :: String
    , clientJoinCount :: TVar ClientJoinID
    , roomRefCount    :: TVar ChatroomRef
    , roomNameToRef   :: TVar (M.Map ChatroomName ChatroomRef)
    , serverChatrooms :: TVar (M.Map ChatroomRef Chatroom)
    , nameToJoinId    :: TVar (M.Map ClientName ClientJoinID)
    , serverClients   :: TVar (M.Map ClientJoinID Client)
    }

newChatServer :: String -> String -> IO ChatServer
newChatServer address port = atomically $ do
    ChatServer <$> return address <*> return port <*> newTVar 0 <*> newTVar 0 <*> newTVar M.empty
               <*> newTVar M.empty <*> newTVar M.empty <*> newTVar M.empty

addChatroom :: ChatServer -> ChatroomName -> ChatroomRef -> STM ()
addChatroom ChatServer{..} name roomRef = do
    room <- newChatroom name roomRef
    modifyTVar serverChatrooms . M.insert roomRef $ room
    modifyTVar roomNameToRef . M.insert name $ roomRef

lookupChatroomByName :: ChatServer -> ChatroomName -> STM (Maybe Chatroom)
lookupChatroomByName ChatServer{..} name = do
    roomRef <- M.lookup name <$> readTVar roomNameToRef
    case roomRef of
        Nothing  -> return Nothing
        Just ref -> M.lookup ref <$> readTVar serverChatrooms

lookupChatroomByRef :: ChatServer -> ChatroomRef -> STM (Maybe Chatroom)
lookupChatroomByRef ChatServer{..} roomRef = M.lookup roomRef <$> readTVar serverChatrooms

lookupOrCreateChatroom :: ChatServer -> ChatroomName -> STM Chatroom
lookupOrCreateChatroom server@ChatServer{..} name = lookupChatroomByName server name >>= \x ->
    case x of
      Nothing -> do
        roomRef <- readTVar roomRefCount
        room <- newChatroom name roomRef
        modifyTVar serverChatrooms . M.insert roomRef $ room
        modifyTVar roomNameToRef . M.insert name $ roomRef
        incrementRoomRefCount roomRefCount
        return room
      Just room -> return room

addClientToServer :: ChatServer -> ClientJoinID -> Client -> STM ()
addClientToServer server@ChatServer{..} joinID client@Client{..} =
    modifyTVar serverClients $ M.insert joinID client

removeClientFromServer :: ChatServer -> ClientJoinID -> STM ()
removeClientFromServer server@ChatServer{..} joinID =
    modifyTVar serverClients $ M.delete joinID
	
{-
    Chatroom
-}
data Chatroom = Chatroom
    { chatroomName          :: ChatroomName
    , chatroomRef           :: ChatroomRef
    , chatroomClients       :: TVar (M.Map ClientJoinID Handle)
    }

newChatroom :: ChatroomName -> ChatroomRef -> STM Chatroom
newChatroom name ref = Chatroom name <$> return ref <*> newTVar M.empty

chatroomAddClient :: Chatroom -> ClientJoinID -> Handle -> STM ()
chatroomAddClient room joinID handle = modifyTVar (chatroomClients room) . M.insert joinID $ handle

chatroomRemoveClient :: Chatroom -> ClientJoinID -> STM ()
chatroomRemoveClient room joinID = modifyTVar (chatroomClients room) $ M.delete joinID

chatroomGetRef :: Chatroom -> ChatroomRef
chatroomGetRef Chatroom{..} = chatroomRef

ipAddress :: String
ipAddress = "10.62.0.217"

data Client = Client
    { clientName          :: TVar ClientName
    , clientJoinID        :: ClientJoinID
    , clientHandle        :: Handle
    , clientRoomRefs      :: TVar (S.Set ChatroomRef)
    }

newClient :: ClientJoinID -> Handle -> STM Client
newClient joinID handle = do
    clientName <- newTVar "default"
    roomRefs <- newTVar S.empty
    return Client
        { clientName          = clientName
        , clientJoinID        = joinID
        , clientHandle        = handle
        , clientRoomRefs      = roomRefs
        }

clientChangeName :: Client -> ClientName -> STM ()
clientChangeName client@Client{..} name = writeTVar clientName name

clientHandler :: Handle -> Chan String -> ChatServer -> IO ()
clientHandler handle chan server@ChatServer{..} = do
        msg <- hGetLine handle
        print $ msg ++ "!ENDLINE!"
        let cmd = head $ words $ head $ splitOn ":" msg
        print cmd
        case cmd of
            ("JOIN_CHATROOM") -> joinCommand handle server msg
            ("CHAT") -> messageCommand handle server msg
            ("LEAVE_CHATROOM") -> leaveCommand handle server msg
            ("DISCONNECT") -> terminateCommand handle server msg
            ("HELO") -> heloCommand handle server $ (words msg) !! 1
            ("KILL_SERVICE") -> killCommand chan handle
            _ -> do hPutStrLn handle ("Unknown Command - " ++ msg ++ "\n\n") ; return ()

joinCommand :: Handle -> ChatServer -> String -> IO ()
joinCommand handle server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        chatroomName = (splitOn ":" $ clines !! 0) !! 1
        clientNamer = (splitOn ":" $ clines !! 3) !! 1

    joinID <- atomically $ readTVar clientJoinCount
    c <- atomically $ newClient joinID handle
    atomically $ addClientToServer server joinID c
    atomically $ incrementClientJoinCount clientJoinCount

    room <- atomically $ lookupOrCreateChatroom server chatroomName
    atomically $ chatroomAddClient room joinID handle

    hPutStrLn handle $ "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++
                       "SERVER_IP:" ++ ipAddress ++ "\n" ++
                       "PORT:" ++ port ++ "\n" ++
                       "ROOM_REF:" ++ show (chatroomGetRef room) ++ "\n" ++
                       "JOIN_ID:" ++ show joinID
    hFlush handle
    clients <- atomically $ readTVar $ chatroomClients room
    let sockList = map snd $ M.toList clients
    let roomref = chatroomGetRef room
    let msg = "CHAT:" ++ (show roomref) ++ "\n" ++ 
              "CLIENT_NAME:" ++ clientNamer ++ "\n" ++ 
              "MESSAGE:" ++ "joined!"
    mapM_ (sendcrmsg msg) sockList   
    return ()

sendcrmsg :: String -> Handle -> IO()
sendcrmsg msg handle = do
    hSetBuffering handle (BlockBuffering Nothing)
    hPutStrLn handle $ msg
    hFlush handle

messageCommand :: Handle -> ChatServer -> String -> IO ()
messageCommand handle server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        chatroomRef = (splitOn ":" $ clines !! 0) !! 1
        joinID = (splitOn ":" $ clines !! 1) !! 1
        clientName = (splitOn ":" $ clines !! 2) !! 1
        message = (splitOn ":" $ clines !! 3) !! 1

    print(chatroomRef)
    room <- atomically $ lookupChatroomByRef server $ read chatroomRef
    case room of
        Nothing -> do hPutStrLn handle ("The room you have messaged does not exist!")
                      hFlush handle
        Just room -> do
            clients <- atomically $ readTVar $ chatroomClients room
            let sockList = map snd $ M.toList clients
            print sockList
            let msg = "CHAT:" ++ chatroomRef ++ "\n" ++ "CLIENT_NAME:" ++ clientName ++ "\n" ++ "MESSAGE:" ++ show message ++ "\n\n"
            mapM_ (\s -> hPutStrLn s $ msg) sockList
            hFlush handle

leaveCommand :: Handle -> ChatServer -> String -> IO ()
leaveCommand handle server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        chatroomRef = (splitOn ":" $ clines !! 0) !! 1
        joinID = (splitOn ":" $ clines !! 1) !! 1
        clientName = (splitOn ":" $ clines !! 2) !! 1
    
    room <- atomically $ lookupChatroomByRef server $ read chatroomRef

    case room of
        (Just r) -> do
            atomically $ chatroomRemoveClient r (read joinID)
                    
            hPutStrLn handle $ 
                "LEFT_CHATROOM:" ++ chatroomRef ++ "\n" ++
                "JOIN_ID:" ++ show joinID ++ "\n\n"

            return ()
            
        Nothing  -> do
            hPutStrLn handle $ "Chatroom you have tried to leave does not exist."
            hFlush handle

terminateCommand :: Handle -> ChatServer -> String -> IO ()
terminateCommand handle server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        address = (splitOn ":" $ clines !! 0) !! 1
        port = (splitOn ":" $ clines !! 1) !! 1
        clientName = (splitOn ":" $ clines !! 2) !! 1

    print $ "Client " ++ clientName ++ " removed!"
    hClose handle

heloCommand :: Handle -> ChatServer -> String -> IO ()
heloCommand handle ChatServer{..} msg = do
  hSetBuffering handle (BlockBuffering Nothing)
  hPutStrLn handle $  "HELO " ++ msg ++ "\n" ++
                      "IP:" ++ ipAddress ++ "\n" ++
                      "Port:" ++ port ++ "\n" ++
                      "StudentID:12306421\n\n"

  hFlush handle

killCommand :: Chan String -> Handle -> IO ()
killCommand chan handle = do
    hPutStrLn handle $  "Service is now terminating!"
    writeChan chan "KILL_SERVICE"

incrementClientJoinCount :: TVar ClientJoinID -> STM ()
incrementClientJoinCount tv = modifyTVar tv ((+) 1)

incrementRoomRefCount :: TVar ChatroomRef -> STM ()
incrementRoomRefCount tv = modifyTVar tv ((+) 1)
