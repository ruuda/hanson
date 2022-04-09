{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class as Default
import Data.Text as Text
import Web.Scotty as Scotty

import Text.Blaze (Markup)

import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

renderMarket :: Int -> Markup
renderMarket i = Html.article $ do
  Html.h1 $ Blaze.text $ "Market " <> (Text.pack $ show i)
  Html.p $ Blaze.text "This is a market."

serveHtml :: Markup -> Scotty.ActionM ()
serveHtml markup = do
  Scotty.setHeader "Content-Type" "text/html; charset=utf-8"
  Scotty.raw $ BlazeUtf8.renderHtml markup

main :: IO ()
main = Scotty.scottyOpts Default.def $ do

  Scotty.get "/market/:market_id" $ do
    marketId <- Scotty.param "market_id"
    serveHtml $ renderMarket $ marketId
    liftIO $ putStrLn $ "Hi from market."
  
