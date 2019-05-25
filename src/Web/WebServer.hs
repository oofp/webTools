module Web.WebServer
  ( runTlsApp
  , runTlsExApp
  , runHttpApp
  , runHttpAppWithPath
  , runHttpExApp
  , runRedirect
  , runStaticHttpSrv
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

noScottyApps :: Scotty.ScottyM ()
noScottyApps = return ()

scottyHandlerWithPath :: String -> String -> Scotty.ScottyM () -> Scotty.ScottyM ()
scottyHandlerWithPath defPage rootPath extraApps = do
   Scotty.get "/" $ Scotty.file defPage
   extraApps
   Scotty.middleware $ staticPolicy (noDots >-> addBase rootPath)

scottyHandler :: String -> Scotty.ScottyM () -> Scotty.ScottyM ()
scottyHandler defPage extraApps = scottyHandlerWithPath defPage "static" extraApps  

waiScottyAppWithPath :: String -> String -> Scotty.ScottyM () ->IO WAI.Application
waiScottyAppWithPath defPage rootPath webApps = Scotty.scottyApp (scottyHandlerWithPath defPage rootPath webApps)
   
waiScottyApp :: String -> Scotty.ScottyM () ->IO WAI.Application
waiScottyApp defPage webApps = Scotty.scottyApp (scottyHandler defPage webApps)

webAppWithPath :: String -> String -> Scotty.ScottyM () -> WS.ServerApp->IO WAI.Application
webAppWithPath defPage rootPath webApps wsApp = do
  httpHandler <- waiScottyAppWithPath defPage rootPath webApps
  return $ websocketsOr defaultConnectionOptions wsApp httpHandler

webApp :: String ->  Scotty.ScottyM () -> WS.ServerApp->IO WAI.Application
webApp defPage webApps wsApp = do
  httpHandler <- waiScottyApp defPage webApps
  return $ websocketsOr defaultConnectionOptions wsApp httpHandler

staticWebAppWithPath :: String -> String -> IO WAI.Application
staticWebAppWithPath defPage rootPath = waiScottyAppWithPath defPage rootPath noScottyApps
    
runTlsApp :: String -> TLSSettings -> WS.ServerApp->Int->IO ()
runTlsApp  defPage theTlsSettings wsApp port = do
   let cfg=Warp.setPort port Warp.defaultSettings
   appToRun <- webApp defPage noScottyApps wsApp
   runTLS theTlsSettings cfg appToRun

runTlsExApp :: String -> Scotty.ScottyM () -> TLSSettings -> WS.ServerApp->Int->IO ()
runTlsExApp  defPage webApps theTlsSettings wsApp port = do
  let cfg=Warp.setPort port Warp.defaultSettings
  appToRun <- webApp defPage webApps wsApp
  runTLS theTlsSettings cfg appToRun

runHttpApp :: String -> WS.ServerApp->Int->IO ()
runHttpApp  defPage wsApp port = do
  appToRun <- webApp defPage noScottyApps wsApp
  Warp.run port appToRun

runHttpAppWithPath :: String -> String -> WS.ServerApp->Int->IO ()
runHttpAppWithPath defPage rootPath wsApp port = do
  appToRun <- webAppWithPath defPage rootPath noScottyApps wsApp
  Warp.run port appToRun

runStaticHttpSrv :: String -> String -> Int -> IO ()
runStaticHttpSrv defPage rootPath port = do
  appToRun <- staticWebAppWithPath defPage rootPath 
  Warp.run port appToRun
  
runHttpExApp :: String -> Scotty.ScottyM () -> WS.ServerApp->Int->IO ()
runHttpExApp  defPage webApps wsApp port = do
  appToRun <- webApp defPage webApps wsApp
  Warp.run port appToRun

redirApp :: String -> WAI.Application
redirApp toURL _req respond = respond =<< redirect' HTTPTypes.status302 [] uri
 where
   -- Just uri = parseURI "https://localhost:443/GroupCalls.html"
   -- Just uri = parseURI "https://voip2.me/GroupCalls.html"
   Just uri = parseURI toURL

runRedirect :: Int -> String -> IO ()
runRedirect port toURL = Warp.run port (redirApp toURL)
