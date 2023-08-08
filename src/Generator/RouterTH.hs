{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Generator.RouterTH (genR) where

import Data.Int (Int64)
import Language.Haskell.TH
import Servant
import Server.ServerGen
import Data.ByteString.Lazy qualified as B
import Data.Aeson
import System.Directory (getDirectoryContents)
import Data.List (isSuffixOf)
import Language.Haskell.TH()
import Language.Haskell.TH.Syntax()
import Generator.Model as GM

mkRouteType :: (String, String, Q Type) -> Q Type
mkRouteType (routePrefix, routeName, entityTable) =
  [t| $(litT (strTyLit routePrefix)) :> $(litT (strTyLit routeName)) :> APIGen $entityTable Int64 |]
  -- ("api", "foo", Bar)

combineRoutes :: Q Type -> Q Type -> Q Type
combineRoutes qtype1 qtype2 = [t| $qtype1 :<|> $qtype2|]

mkRoutes :: [(String, String, Q Type)] -> Q Type
mkRoutes routeList = foldr1 combineRoutes (map mkRouteType routeList)

routeTH :: GM.Model -> (String, String, Q Type)
routeTH (GM.Model name prefix entity _) = (prefix, name, conT (mkName entity))

readFileContent :: String -> Q (String, String, Q Type)
readFileContent file = do
  jsonFile <- runIO (B.readFile file)
  let maybeModel = eitherDecode jsonFile :: Either String Model
  case maybeModel of
    Left err -> error $ "Error decoding JSON file (RouterTH): " ++ err
    Right model -> return (routeTH model)

getRoutes :: Q [String]
getRoutes = do
  files <- runIO $ do
    dirContents <- getDirectoryContents "config/crud/"
    pure $ map ("config/crud/" <>) $ filter (".json" `isSuffixOf`) dirContents
  return files

genR :: Q Type
genR = do
  files <- getRoutes
  routeTypes <- mapM readFileContent files
  mkRoutes routeTypes