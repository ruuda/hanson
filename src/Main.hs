{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class as Default
import Data.Text.Lazy as LazyText
import Web.Scotty as Scotty

main :: IO ()
main = Scotty.scottyOpts Default.def $ do

  Scotty.get "/market/:market_id" $ do
    marketId <- Scotty.param "market_id"
    Scotty.text $ "Market " <> (LazyText.pack $ show @Int marketId)
    liftIO $ putStrLn $ "Hi from market."
  
