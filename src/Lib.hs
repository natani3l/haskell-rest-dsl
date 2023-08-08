module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Server.Routes

startApp :: IO ()
startApp = do
    let port = 8080
    putStrLn $ "Server running on port: " ++ show port
    run port app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy