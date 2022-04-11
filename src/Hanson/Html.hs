{-# LANGUAGE OverloadedStrings #-}

module Hanson.Html
 ( renderBase
 , renderForAnonymousVisitor
 )
where

import Data.Text (Text)
import Text.Blaze (Markup, (!))

import qualified Text.Blaze as Blaze
import qualified Text.Blaze.Html5 as Html
import qualified Text.Blaze.Html5.Attributes as Attr

-- Render the base page template, given the title and contents for the header
-- bar and body.
renderBase :: Text -> Markup -> Markup -> Markup
renderBase title headerbar body =
  Html.docTypeHtml $ do
    Html.head $ do
      Html.meta
        ! Attr.charset "utf-8"
      Html.meta
        ! Attr.name "viewport"
        ! Attr.content "width=device-width, initial-scale=1"
      Html.title $ Blaze.text title
      Html.link
        ! Attr.href "/static/style.css"
        ! Attr.rel "stylesheet"
        ! Attr.type_ "text/css"

    Html.body $ do
      Html.header
        ! Attr.id "header"
        ! Attr.class_ "columns"
        $ headerbar

      Html.section
        ! Attr.id "main"
        ! Attr.class_ "columns"
        $ body

-- Render a page where the header bar does not contain any controls for the
-- session user, only a link to the home page.
renderForAnonymousVisitor :: Text -> Markup -> Markup
renderForAnonymousVisitor title body = renderBase title headerbar body
  where
    headerbar =
      Html.nav ! Attr.id "main-nav" $
        Html.a ! Attr.href "/" ! Attr.id "home-link" $ Blaze.text "Hanson"
