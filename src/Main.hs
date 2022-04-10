{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Only (..))
import Text.Blaze (Markup)
import Data.Text (Text)
import Database.PostgreSQL.Simple.Transaction (ReadWriteMode (..))

import qualified Data.Default.Class as Default
import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Web.Scotty as Scotty

import qualified Hanson.Database as Db

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
  pool <- Db.newConnectionPool

  Scotty.scottyOpts Default.def $ do
    Scotty.get "/market/:market_id" $ do
      marketId :: Int <- Scotty.param "market_id"

      title <- liftIO $ Db.withConnection pool $ \conn -> do
        Db.withTransaction ReadOnly conn $ \tx -> do
          [Only (title :: Text)] <- Db.query tx "SELECT market_current_title(?);" (Only marketId)
          Db.commit tx
          pure title

      serveHtml $ renderMarket $ title
      liftIO $ putStrLn $ "Hi from market."
  
