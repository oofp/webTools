module Web.WebServer
  ( runTlsApp
  , runTlsExApp
  , runHttpApp
  , runHttpExApp
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

noScottyApps :: Scotty.ScottyM ()
noScottyApps = return ()

scottyHandler :: String -> Scotty.ScottyM () -> Scotty.ScottyM ()
scottyHandler defPage extraApps = do
   Scotty.get "/" $ Scotty.file defPage
   extraApps
   Scotty.middleware $ staticPolicy (noDots >-> addBase "static")

waiScottyApp :: String -> Scotty.ScottyM () ->IO WAI.Application
waiScottyApp defPage webApps = Scotty.scottyApp (scottyHandler defPage webApps)

webApp :: String ->  Scotty.ScottyM () -> WS.ServerApp->IO WAI.Application
webApp defPage webApps wsApp = do
  httpHandler <- waiScottyApp defPage webApps
  return $ websocketsOr defaultConnectionOptions wsApp httpHandler

runTlsApp :: String -> TLSSettings -> WS.ServerApp->Int->IO ()
runTlsApp  defPage theTlsSettings wsApp port = do
   --let tlsCfg=tlsSettingsChain "/etc/letsencrypt/live/sip.uno/cert.pem" ["/etc/letsencrypt/live/sip.uno/chain.pem"] "/etc/letsencrypt/live/sip.uno/privkey.pem"
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

runHttpExApp :: String -> Scotty.ScottyM () -> WS.ServerApp->Int->IO ()
runHttpExApp  defPage webApps wsApp port = do
  appToRun <- webApp defPage webApps wsApp
  Warp.run port appToRun

redirApp :: URI -> WAI.Application
redirApp url _req respond = respond =<< redirect' HTTPTypes.status302 [] url
 --where
 --   Just uri = parseURI toURL

runRedirect :: Int -> String -> IO (Maybe ())
runRedirect port toURL =
  let urlMaybe = parseURI toURL
  in forM urlMaybe (Warp.run port . redirApp)
