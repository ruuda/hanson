{-# LANGUAGE OverloadedStrings #-}

module Hanson.Route.Session (routes) where

import Text.Blaze (Markup, (!))

import qualified Web.Scotty as Scotty
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

import Hanson.Http (respondHtml)
import Hanson.Html (renderForAnonymousVisitor)

pageLogin :: Markup
pageLogin =
  renderForAnonymousVisitor "Log In — Hanson" $ do
    Html.div ! Attr.id "login-form" $ do
      Html.form ! Attr.method "post" $ do
        Html.label
          ! Attr.for "username"
          $ Blaze.text "Username"
        Html.input
          ! Attr.name "username"
          ! Attr.id "username"
          ! Attr.required mempty
        -- For now there is no password field, no authentication.
        -- Eventually, I would rather use a third-party identity provider.
        Html.input
          ! Attr.type_ "submit"
          ! Attr.value "Log In"

routes :: Scotty.ScottyM ()
routes = do
  Scotty.get "/login" $ respondHtml pageLogin

  Scotty.post "/login" $ do
    Scotty.text "TODO: Handle post login form"

  Scotty.get "/logout" $ do
    Scotty.text "TODO: Logout form"

  Scotty.post "/logout" $ do
    Scotty.text "TODO: Handle post logout form"
