module Main where

import Protolude
import Web.WebServer
import Web.WSServer

import qualified Pipes as P
import qualified Pipes.Concurrent as PC
import Pipes ((>->))
import qualified Data.Aeson as DA

newClientHandler :: NewClientCallback DA.Value DA.Value
newClientHandler (input, output) =
  void $ async $ P.runEffect $ PC.fromInput input  >-> PC.toOutput output


main :: IO ()
main = do
  wsApp <- getWSApp newClientHandler
  httpTask <- async $ runHttpApp "sample.html" wsApp 8070

  --let tlsCfg=tlsSettingsChain "cert1.pem" ["chain1.pem"] "privkey1.pem"
  --httpTask <- async $ runTlsApp  "static/sample.html" tlsCfg wsApp 4430

  putStrLn ("Press [Enter] to exit"::Text)
  void getLine
  cancel httpTask
