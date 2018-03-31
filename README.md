# webTools

Main.hs contains simple example that start HTTP server with web socket application that echo received packets back to the sender.

```
newClientHandler :: NewClientCallback DA.Value DA.Value
newClientHandler (input, output) =
  void $ async $ P.runEffect $ PC.fromInput input  >-> PC.toOutput output


main :: IO ()
main = do
  wsApp <- getWSApp newClientHandler
  httpTask <- async $ runHttpApp "sample.html" wsApp 8070

  putStrLn ("Press [Enter] to exit"::Text)
  void getLine
  cancel httpTask
```

To use HTTPs replace  `httpTask <- async $ runHttpApp "sample.html" wsApp 8070` by:
```
let tlsCfg=tlsSettingsChain "cert1.pem" ["chain1.pem"] "privkey1.pem"
httpTask <- async $ runTlsApp  "static/sample.html" tlsCfg wsApp 443
```

In both cases the default page is passed as parameter ("sample.html"). HTTP server also server static pages from ./static directory

To test WebSocket, elm demo client can be used at: http://elm-lang.org/examples/websockets
Just send URL to address of your server (ws://localhost:8070, for example above). Please note that to get it working request should be entered as valid JSON. For example:
`{"array":[1,2,3],"element":"value"}`

To server other HTTP(s) requests (for example REST) in addition to static pages use the following methods:
```
runTlsExApp :: String -> Scotty.ScottyM () -> TLSSettings -> WS.ServerApp->Int->IO ()
runTlsExApp  defPage webApps theTlsSettings wsApp port = do

runHttpExApp :: String -> Scotty.ScottyM () -> WS.ServerApp->Int->IO ()
runHttpExApp  defPage webApps wsApp port = do
```
  Both functions allow accept ScottyM as extra parameter

To redirect all incoming requests on specific port use:
```
runRedirect :: Int -> String -> IO (Maybe ())
runRedirect port toURL
```
