{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model.Migration where

import Common.Database
import Control.Monad.IO.Class (liftIO)
import Data.Aeson hiding (Key)
import Data.List (isSuffixOf)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Quasi.Internal
import Database.Persist.Sql
import qualified Database.Persist.TH as PTH
import GHC.Generics
import System.Directory (getDirectoryContents)

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  $( do
       files <- liftIO $ do
         dirContents <- getDirectoryContents "config/models/"
         pure $ map ("config/models/" <>) $ filter (".model" `isSuffixOf`) dirContents
       PTH.persistManyFileWith lowerCaseSettings files
   )

migrateModel :: DBInfo -> IO ()
migrateModel connString = runAction connString (runMigration migrateAll)
