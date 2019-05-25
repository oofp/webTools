module Main where

import Protolude
import Web.WebServer

main :: IO ()
main = do
  putStrLn ("Welcome to StaticHTTPSrv"::Text)
  args <- getArgs
  case args of 
    [defPage, path, portTxt] ->
      let srvPortMaybe :: Maybe Int
          srvPortMaybe = readMaybe portTxt
      in case srvPortMaybe of
        Just srvPort -> runStaticHttpSrv defPage path srvPort
        Nothing ->
          putStrLn ("Failed to parse port (must be numeric)"::Text)
    _ -> do putStrLn ("Parametrs: defPage rootPath httpPort"::Text)        
