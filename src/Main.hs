{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Only (..), connectPostgreSQL)
import Text.Blaze (Markup)
import Data.Text (Text)

import qualified Data.Default.Class as Default
import qualified Data.Text as Text
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Web.Scotty as Scotty

renderMarket :: Text -> Markup
renderMarket title = Html.article $ do
  Html.h1 $ Blaze.text $ "Market " <> title
  Html.p $ Blaze.text "This is a market."

serveHtml :: Markup -> Scotty.ActionM ()
serveHtml markup = do
  Scotty.setHeader "Content-Type" "text/html; charset=utf-8"
  Scotty.raw $ BlazeUtf8.renderHtml markup

main :: IO ()
main = do
  conn <- connectPostgreSQL ""

  Scotty.scottyOpts Default.def $ do
    Scotty.get "/market/:market_id" $ do
      marketId :: Int <- Scotty.param "market_id"
      [Only (title :: Text)] <- liftIO $ Postgres.query conn "SELECT market_current_title(?);" (Only marketId)
      serveHtml $ renderMarket $ title
      liftIO $ putStrLn $ "Hi from market."
  
