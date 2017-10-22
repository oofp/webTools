module Web.WebServer
  ( runTlsApp
  , runHttpApp
  , runRedirect
  ) where

import Protolude

import qualified Network.Wai as WAI
import Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.HTTP.Types as HTTPTypes
import Network.Wai.Handler.WarpTLS
import qualified Web.Scotty as Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Util
import Network.URI
import Prelude (String)

scottyHandler :: String -> Scotty.ScottyM ()
scottyHandler defPage = do
   Scotty.get "/" $ Scotty.file defPage
   Scotty.get "/:path" $ Scotty.file defPage
   Scotty.middleware $ staticPolicy (noDots >-> addBase "static")

waiScottyApp :: String -> IO WAI.Application
waiScottyApp defPage = Scotty.scottyApp (scottyHandler defPage)

webApp :: String -> WS.ServerApp->IO WAI.Application
webApp defPage wsApp = do
  httpHandler <- waiScottyApp defPage
  return $ websocketsOr defaultConnectionOptions wsApp httpHandler

runTlsApp :: String -> TLSSettings -> WS.ServerApp->Int->IO ()
runTlsApp  defPage theTlsSettings wsApp port = do
   let cfg=Warp.setPort port Warp.defaultSettings
   appToRun <- webApp defPage wsApp
   runTLS theTlsSettings cfg appToRun

runHttpApp :: String -> WS.ServerApp->Int->IO ()
runHttpApp  defPage wsApp port = do
  appToRun <- webApp defPage wsApp
  Warp.run port appToRun


redirApp :: String -> WAI.Application
redirApp toURL _req respond = respond =<< redirect' HTTPTypes.status302 [] uri
 where
   Just uri = parseURI toURL

runRedirect :: Int -> String -> IO ()
runRedirect port toURL = Warp.run port (redirApp toURL)
