{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Routes
  ( API,
    server,
  )
where

import Generator.RouterTH
import Model.Migration
import Servant
import Generator.ServerTH

type API = RoutesAPI

server :: Server API
server = serverAuth

type RoutesAPI = $(genR)

serverAuth :: Server RoutesAPI
serverAuth = $(genS)