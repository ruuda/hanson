{-# LANGUAGE OverloadedStrings #-}

module Hanson.Http
 ( respondHtml
 )
where

import Text.Blaze (Markup)

import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import qualified Web.Scotty as Scotty

respondHtml :: Markup -> Scotty.ActionM ()
respondHtml markup = do
  Scotty.setHeader "Content-Type" "text/html; charset=utf-8"
  Scotty.raw $ BlazeUtf8.renderHtml markup
