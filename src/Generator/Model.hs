{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Generator.Model (generateApp, Model (Model)) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString.Lazy qualified as B
import Data.Char
import Data.List (isSuffixOf)
import GHC.Generics
import System.Directory (getDirectoryContents)

capitalized :: String -> String
capitalized (h : t) = toUpper h : map toLower t
capitalized [] = []

data Model = Model
  { name :: String
  , prefix :: String
  , entity :: String
  , fields :: [Field]
  }
  deriving (Show, Generic)

instance FromJSON Model

instance ToJSON Model

data Field = Field
  { fieldName :: String,
    fieldType :: String
  }
  deriving (Show, Generic)

instance FromJSON Field

instance ToJSON Field

saveModel :: Model -> IO ()
saveModel m@(Model modelName _ _ _) = do
  let fileName = "config/models/" ++ capitalized modelName ++ ".model"
  writeFile fileName $ exportModel m

exportField :: Field -> String
exportField (Field n t) = "        " ++ n ++ " " ++ capitalized t ++ "\n"

exportModel :: Model -> String
exportModel (Model n _ _ f) = "    " ++ capitalized n ++ " sql\n" ++ concatMap exportField f ++ "      deriving Show Read Generic ToJSON FromJSON"

generateModel :: String -> IO ()
generateModel file = do
  jsonFile <- B.readFile file
  let maybeModels = eitherDecode jsonFile :: Either String Model
  case maybeModels of
    Left err -> putStrLn $ "Error JSON: " ++ err
    Right model -> saveModel model

generateApp :: IO ()
generateApp = do
  files <- liftIO $ do
    dirContents <- getDirectoryContents "config/crud/"
    pure $ map ("config/crud/" <>) $ filter (".json" `isSuffixOf`) dirContents
  print files

  mapM_ generateModel files
