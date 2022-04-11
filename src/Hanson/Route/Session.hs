{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hanson.Route.Session (routes) where

import Control.Monad.IO.Class (liftIO)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Transaction (ReadWriteMode (..))
import Text.Blaze (Markup, (!))

import qualified Web.Scotty as Scotty
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

import Hanson.Html (renderForAnonymousVisitor)
import Hanson.Model.User (User)

import qualified Hanson.Database as Db
import qualified Hanson.Http as Http
import qualified Hanson.Model.User as User

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

routes :: Pool Connection -> Scotty.ScottyM ()
routes pool = do
  Scotty.get "/login" $
    Http.respondHtml pageLogin

  Scotty.post "/login" $ do
    -- TODO: We should really return 400 if the param is not present, not 500.
    username <- Scotty.param "username"

    liftIO $ Db.withPoolTransaction ReadWrite pool $ \tx -> do
      u <- User.getByUsername tx username
      case u of
        Just uu -> putStrLn $ "Found user: " <> (show uu)
        Nothing -> putStrLn $ "No such user."
      -- TODO: Create the session if we have a user.
      Db.commit tx

    -- TODO: Actually log the user in
    Http.respondSeeOther "/"

  Scotty.get "/logout" $ do
    Scotty.text "TODO: Logout form"

  Scotty.post "/logout" $ do
    Scotty.text "TODO: Handle post logout form"
