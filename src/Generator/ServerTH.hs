
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Generator.ServerTH (genS) where

import Data.Int (Int64)
import Language.Haskell.TH
import Servant
import Common.Database
import Server.ServerGen
import Data.ByteString.Lazy qualified as B
import Data.Aeson
import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import Language.Haskell.TH()
import Language.Haskell.TH.Syntax()
import Generator.Model as GM

serverGenExpr :: Q Type -> Q Exp
serverGenExpr typeExpr =
    [e| serverGen
          (listHandlerGen connectionString :: Handler [$(typeExpr)])
          (newHandlerGen connectionString :: $(typeExpr) -> Handler Int64)
          (viewHandlerGen connectionString :: Int64 -> Handler $(typeExpr))
          (updateHandlerGen connectionString :: Int64 -> $(typeExpr) -> Handler ())
          (deleteHandlerGen connectionString (Proxy :: Proxy $(typeExpr)) :: Int64 -> Handler ())
    |]

combineServer :: Q Exp -> Q Exp -> Q Exp
combineServer qexp1 qexp2 = [e| $(qexp1) :<|> $(qexp2) |]

mkServer :: [Q Type] -> Q Exp
mkServer serverList = foldr1 combineServer (map serverGenExpr serverList)

serverTH :: Model -> Q Type
serverTH (Model _ _ entity _) = conT (mkName entity)


readFileContent :: String -> Q (Q Type)
readFileContent file = do
  jsonFile <- runIO (B.readFile file)
  let maybeModel = eitherDecode jsonFile :: Either String Model
  case maybeModel of
    Left err -> error $ "Error decoding JSON file (ServerTH): " ++ err
    Right model -> return (serverTH model)

getServer :: Q [String]
getServer = runIO $ do
  dirContents <- getDirectoryContents "config/crud/"
  let files = map ("config/crud/" <>) $ filter (".json" `isSuffixOf`) dirContents
  return files

genS :: Q Exp
genS = do
  files <- getServer
  serverTypes <- mapM readFileContent files
  mkServer serverTypes
