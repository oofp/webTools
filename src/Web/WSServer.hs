--This is the Haskell implementation of the example for the WebSockets library. We
--implement a simple multi-user chat program. A live demo of the example is
--available [here](http://jaspervdj.be/websockets-example). In order to understand
--this example, keep the [reference](http://jaspervdj.be/websockets/reference)
--nearby to check out the functions we use.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes  #-}

module Web.WSServer
    ( startWSServer
    , getWSApp
    , InputOutput
    , NewClientCallback
    )
    where
--import qualified Data.Text as T
import Protolude hiding (for)
import Prelude (String)
import qualified Data.UUID.V4 as UUIDGen
import Pipes ((>->), runEffect, for, yield, await)
import qualified Pipes.Concurrent as PC
import qualified Pipes
import Data.Aeson
import qualified Data.Aeson as DA
import System.Log.Logger
import qualified Pipes.Aeson as PA
import qualified Pipes.Aeson.Unchecked as PAU
import qualified Network.WebSockets as WS
import Control.Lens (view)
import qualified  Data.ByteString.Lazy as BStrL

loggerPath::String
loggerPath="WSServer"

type Client = (String, WS.Connection)

--The state kept on the server is simply a list of connected clients. We've added
--an alias and some utility functions, so it will be easier to extend this state
--later on.

type ServerState = [Client]

--Create a new, initial state:
newServerState :: ServerState
newServerState = []

--Get the number of active clients:
--numClients :: ServerState -> Int
--umClients = length

--Check if a user already exists (based on username):
--clientExists :: Client -> ServerState -> Bool
--clientExists client = any ((== fst client) . fst)

--Add a client (this does not check if the client already exists, you should do
--this yourself using `clientExists`):
addClient :: Client -> ServerState -> ServerState
addClient client clients = client : clients

--Remove a client:
removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

--Send a message to all clients, and log it on stdout:
--broadcast :: Text -> ServerState -> IO ()
--broadcast message clients = do
--     T.putStrLn message
--     forM_ clients $ \(_, conn) -> WS.sendTextData conn message

--The main function first creates a new state for the server, then spawns the
--actual server. For this purpose, we use the simple server provided by
--`WS.runServer`.

type InputOutput i o = (PC.Input i,PC.Output o)
type NewClientCallback i o = InputOutput i o -> IO ()

startWSServer :: (ToJSON o, FromJSON i, ToJSON i) => NewClientCallback i o -> Int -> IO ()
startWSServer newClientCallback port= do
    serverState <- newMVar newServerState
    WS.runServer "0.0.0.0" port $ tryApplication newClientCallback serverState

getWSApp :: (ToJSON o, FromJSON i, ToJSON i) => NewClientCallback i o -> IO WS.ServerApp
getWSApp newClientCallback =
  newMVar newServerState >>= (return . tryApplication newClientCallback)

    -- Note that `WS.ServerApp` is nothing but a type synonym for

--Our main application has the type:
tryApplication :: (ToJSON o, FromJSON i, ToJSON i) => NewClientCallback i o -> MVar ServerState -> WS.ServerApp
tryApplication newClientCallback serverState pending = do
  res <- (try $ application newClientCallback serverState pending)::IO (Either SomeException ())
  case res of
    (Left ex) ->  errorM loggerPath ("Exception:" <> show ex)
    (Right _) ->  infoM loggerPath "WS Handling completed"


-- `WS.PendingConnection -> IO ()`.

-- Our application starts by accepting the connection. In a more realistic
--application, you probably want to check the path and headers provided by the
-- pending request.

-- We also fork a pinging thread in the background. This will ensure the connection
-- stays alive on some browsers.
application :: (ToJSON o, FromJSON i, ToJSON i) => NewClientCallback i o -> MVar ServerState -> WS.ServerApp
application newClientCallback serverState  pending= do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

--When a client is succesfully connected, we read the first message. This should
--be in the format of "Hi! I am Jasper", where Jasper is the requested username.

    uuid <- UUIDGen.nextRandom
    let clientID=show uuid
        client=(clientID,conn)
    debugM loggerPath ("Accepting new client"<> clientID)

    (inOut, inIn, sealIn) <- PC.spawn' PC.unbounded
    (outOut, outIn, sealOut) <- PC.spawn' PC.unbounded
    newClientCallback (inIn, outOut)

    forkChannelToConnTask outIn conn>>= (\asyncTask ->
        flip finally (disconnect asyncTask client sealIn sealOut) $ do
         modifyMVar_ serverState $ \s -> do
             let s' = addClient client s
             return s'

         curClients <- readMVar serverState
         let curClientIDs=fst <$> curClients
         infoM loggerPath ("Connected clients:" <> show (length curClientIDs))
         mapM_ (\clID->infoM loggerPath ("Connected client:" <> clID)) curClientIDs

         infoM loggerPath ("Starting recv task clientID:" <> fst client)
         recvRes <- recvTask conn client inOut
         let resStr = case recvRes of
               (Right _) -> " successfully"
               (Left err) -> " error:" <> show (fst err)
         infoM loggerPath ("Recv task completed clientID:" <> fst client <> resStr)
         return ()
        )
    where
      --quitMsg    = (":quit"::Text)
      disconnect asyncTask client sealIn sealOut = do
          -- Remove client and return new state
          cancel asyncTask
          _s <- modifyMVar serverState $ \s ->
              let s' = removeClient client s in return (s', s')
          _ <- atomically (sealIn >> sealOut)
          debugM loggerPath ("Client disconnected"<>fst client)

      forkChannelToConnTask outIn connWs = async $ sendingTask outIn connWs


{-
sendwithDebug connWs dataToSend = do
  debugM loggerPath ("sending data:"<> show dataToSend)
  WS.sendTextData connWs dataToSend
-}

sendJSONData :: ToJSON b => WS.Connection -> b -> IO ()
sendJSONData connWs b =
  WS.sendTextData connWs (BStrL.toStrict (DA.encode b))

sendingTask :: ToJSON b => PC.Input b -> WS.Connection -> IO ()
sendingTask outIn connWs = runEffect $
  PC.fromInput outIn `for` (lift . sendJSONData connWs) --WS.sendTextData connWs)
  --PC.fromInput outIn `for` PAU.encode `for` (lift . sendwithDebug connWs) --WS.sendTextData connWs)

outputConsumer :: PC.Output a -> Pipes.Consumer' a IO r
outputConsumer output = forever $ do
  a <- await
  lift $ atomically $ PC.send output a

bytesProducer :: WS.Connection -> Pipes.Producer ByteString IO ()
bytesProducer conn = forever $ do
  bytes <- lift $ WS.receiveData conn--
  -- liftIO $ debugM loggerPath ("receive bytes: "<> show bytes)
  yield bytes

inProducer :: (ToJSON i, FromJSON i) => Pipes.Producer ByteString IO r -> Pipes.Producer i IO (Either (PA.DecodingError, Pipes.Producer ByteString IO r) r)
inProducer = view PAU.decoded -- bytesProd

inProducerForConn :: (ToJSON i, FromJSON i) => WS.Connection -> Pipes.Producer i IO (Either (PA.DecodingError, Pipes.Producer ByteString IO ()) ())
inProducerForConn  =  inProducer . bytesProducer

recvTask :: (ToJSON i, FromJSON i) => WS.Connection -> Client -> PC.Output i -> IO (Either (PA.DecodingError, Pipes.Producer ByteString IO ()) ())
recvTask conn (user, _) inOut =
  let inputConsumer= outputConsumer inOut
      inputProducer = inProducerForConn conn
  in do
      debugM loggerPath ("start receiving to "<>user)
      runEffect $ inputProducer >-> inputConsumer
