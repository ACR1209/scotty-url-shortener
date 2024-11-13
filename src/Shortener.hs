{-# LANGUAGE OverloadedStrings #-}

module Shortener where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Network.URI (parseURI)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (status404, status400)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import qualified System.Random as SR


type DbConnection = Connection
type Url = (Int, Text, Text)


isValidUrl :: Text -> Bool
isValidUrl input = case parseURI (T.unpack input) of
    Just _  -> True
    Nothing -> False

indexPage :: Text -> [Url] -> ActionM ()
indexPage host urls = do
  html $ renderHtml $
    H.html $
      H.body $ do
        H.h1 "Shortener"
        H.form H.! A.method "post" H.! A.action "/" $ do
          H.input H.! A.type_ "text" H.! A.name "url"
          H.input H.! A.type_ "submit"
        H.table $
          for_ urls $ \(urlId, originalUrl, shortUrl) ->
            H.tr $ do
              H.td (H.toHtml $ show urlId)
              H.td (H.text originalUrl)
              H.td $ H.a H.! A.href (H.toValue $ host <> "/" <> shortUrl) $ H.toHtml (host <> "/" <> shortUrl)


getAllTables :: DbConnection -> IO [Only Text]
getAllTables conn = query_ conn "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"

getAllUrls :: DbConnection -> IO [Url]
getAllUrls conn = query_ conn "SELECT id, original, short_uri FROM url"

insertUrl :: DbConnection -> Text -> IO Int
insertUrl conn url = do
  shortUrl <- randomString 8
  [Only urlId] <- query conn "INSERT INTO url (original, short_uri) VALUES (?, ?) RETURNING id" (url, shortUrl)
  return urlId

getUrlByShortUri :: DbConnection -> Text -> IO (Maybe Url)
getUrlByShortUri conn shortUri = do
  results <- query conn "SELECT id, original, short_uri FROM url WHERE short_uri = ?" (Only shortUri)
  return $ case results of
    [url] -> Just url
    _     -> Nothing

getUrlById :: DbConnection -> Int -> IO (Maybe Text)
getUrlById conn urlId = do
  results <- query conn "SELECT original FROM url WHERE id = ?" (Only urlId)
  return $ case results of
    [Only url] -> Just url
    _          -> Nothing

applyMigrations :: DbConnection -> IO ()
applyMigrations conn = do
  let dir = "migrations"

  tables <- getAllTables conn

  let schemaMigrationsExists = any (== Only "schema_migrations") tables

  if not schemaMigrationsExists
    then do
      _ <- withTransaction conn $ runMigration $ MigrationContext MigrationInitialization True conn
      return ()
    else do
      return ()

  result <- withTransaction conn $ runMigration $ 
    MigrationContext (MigrationDirectory dir) True conn
  liftIO $ print result

randomString :: Int -> IO Text
randomString n = do
  gen <- SR.newStdGen
  let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  return $ T.pack $ take n $ (chars !!) <$> SR.randomRs (0, length chars - 1) gen

shortener :: IO ()
shortener = do
  loadFile defaultConfig
  dbUrl <- getEnv "DATABASE_URL"
  conn <- connectPostgreSQL (encodeUtf8 (T.pack dbUrl))
  host <- T.pack <$> getEnv "HOST"

  applyMigrations conn

  scotty 3000 $ do
    get "/" $ do
      urls <- liftIO $ getAllUrls conn
      indexPage host urls

    post "/" $ do
      url <- formParam "url"
      if not $ isValidUrl url
        then raiseStatus status400 "invalid url"
        else do
          _ <- liftIO $ insertUrl conn url
          redirect "/"
          
    get "/:n" $ do
      n <- captureParam "n"
      url <- liftIO $ getUrlByShortUri conn n
      case url of
        Just (_, originalUrl, _)  -> redirect (LT.fromStrict originalUrl)
        Nothing -> raiseStatus status404 "url not found" 
