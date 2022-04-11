{-# LANGUAGE OverloadedStrings #-}

module Hanson.Http
 ( respondHtml
 , respondSeeOther
 )
where

import Data.Text (Text)
import Text.Blaze (Markup)

import qualified Data.Text.Lazy as LazyText
import qualified Network.HTTP.Types.Status as Status
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import qualified Web.Scotty as Scotty

-- Send 200 OK with html contents.
respondHtml :: Markup -> Scotty.ActionM ()
respondHtml markup = do
  Scotty.setHeader "Content-Type" "text/html; charset=utf-8"
  Scotty.raw $ BlazeUtf8.renderHtml markup

-- Send a 303 See Other. Can be used from a POST, and the browser will do a GET.
respondSeeOther :: Text -> Scotty.ActionM ()
respondSeeOther url = do
  Scotty.setHeader "Location" (LazyText.fromStrict url)
  Scotty.status Status.seeOther303
  Scotty.raw ""
