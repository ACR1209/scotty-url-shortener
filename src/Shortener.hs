{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.URI (parseURI)
import Network.HTTP.Types (status404)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty
import Text.Digestive
import qualified Text.Digestive.Form as F
import Text.Digestive.Types

isValidUrl :: Text -> Bool
isValidUrl input = case parseURI (T.unpack input) of
    Just _  -> True
    Nothing -> False

urlForm :: Monad m => F.Form Text m Text
urlForm = "url" .: check "Invalid URL format" isValidUrl (F.text Nothing)

indexPage :: H.ToMarkup a => Map a Text -> ActionM ()
indexPage urls = do
  html $ renderHtml $
    H.html $
      H.body $ do
        H.h1 "Shortener"
        H.form H.! A.method "post" H.! A.action "/" $ do
          H.input H.! A.type_ "text" H.! A.name "url"
          H.input H.! A.type_ "submit"
        H.table $
          for_ (M.toList urls) $ \(i, url) ->
            H.tr $ do
              H.td (H.toHtml i)
              H.td (H.text url)

shortener :: IO ()
shortener = do
  urlsR <- newIORef (1 :: Int, mempty :: Map Int Text)
  scotty 3000 $ do
    get "/" $ do
      (_, urls) <- liftIO $ readIORef urlsR
      indexPage urls
      
    post "/" $ do
      url <- formParam "url"
      liftIO $ modifyIORef urlsR $
        \(i, urls) ->
          (i + 1, M.insert i url urls)
      redirect "/"
    get "/:n" $ do
      n <- captureParam "n"
      (_, urls) <- liftIO $ readIORef urlsR
      case M.lookup n urls of
        Just url ->
          redirect (LT.fromStrict url)
        Nothing ->
          raiseStatus status404 "not found"