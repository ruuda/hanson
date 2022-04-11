{-# LANGUAGE OverloadedStrings #-}

module Hanson.Route.Session (routes) where

import qualified Web.Scotty as Scotty

import Hanson.Http (respondHtml)

routes :: Scotty.ScottyM ()
routes = do
  Scotty.get "/login" $ do
    Scotty.text "TODO: Login form"

  Scotty.post "/login" $ do
    Scotty.text "TODO: Handle post login form"

  Scotty.get "/logout" $ do
    Scotty.text "TODO: Logout form"

  Scotty.post "/logout" $ do
    Scotty.text "TODO: Handle post logout form"
