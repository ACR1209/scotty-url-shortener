{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Shortener
import qualified Data.Text as T
import Test.QuickCheck 
import Test.Hspec.QuickCheck (prop)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Configuration.Dotenv (loadFile, defaultConfig)
import Data.Text.Encoding (encodeUtf8)


resetDatabase :: IO ()
resetDatabase = do
  loadFile defaultConfig
  connectionStr <- T.pack <$> getEnv "TEST_DATABASE_URL"
  conn <- connectPostgreSQL $ encodeUtf8 connectionStr
  _ <- execute_ conn "SET client_min_messages TO WARNING;"
 
  _ <- execute_ conn "DROP SCHEMA public CASCADE;"
  _ <- execute_ conn "CREATE SCHEMA public;"

  return ()

connectToTestDatabase :: IO DbConnection
connectToTestDatabase = do
  connectionStr <- T.pack <$> getEnv "TEST_DATABASE_URL"
  connectPostgreSQL $ encodeUtf8 connectionStr

main :: IO ()
main = hspec $ describe "Shortener" $ do
  describe "isValidUrl" $ do
    it "returns True for a valid URL" $ do
      isValidUrl "http://example.com" `shouldBe` True
    it "returns True for a valid URL with a path" $ do
      isValidUrl "http://example.com/path" `shouldBe` True
    it "returns True for a valid URL with a query string" $ do
      isValidUrl "http://example.com/path?query=string" `shouldBe` True
    it "returns True for a valid URL with a fragment" $ do
      isValidUrl "http://example.com/path?query=string#fragment" `shouldBe` True
    it "returns True for a valid URL with a port" $ do
      isValidUrl "http://example.com:8080" `shouldBe` True
    it "returns False for an invalid URL" $ do
      isValidUrl "invalid-url" `shouldBe` False
    it "returns False for an empty string" $ do
      isValidUrl "" `shouldBe` False
    it "returns False for a URL with a missing scheme" $ do
      isValidUrl "example.com" `shouldBe` False
    it "returns False for a URL with a missing host" $ do
      isValidUrl "http://" `shouldBe` False
  
  describe "randomString" $ do
    prop "generates a string of the correct length" $ \n -> n >= 0 ==> do
      str <- randomString n
      T.length str `shouldBe` n

    prop "generates a different string each time" $ \n -> n >= 8 ==> do
      str1 <- randomString n
      str2 <- randomString n
      str1 `shouldNotBe` str2

  describe "isWhitespaceOnly" $ do
    prop"returns True for a string with only whitespace" $ do
      isWhitespaceOnly (Just "  \t\n") `shouldBe` True
    it "returns False for a string with non-whitespace characters" $ do
      isWhitespaceOnly (Just "  a\n") `shouldBe` False
    it "returns True for a string with no characters" $ do
      isWhitespaceOnly (Just "") `shouldBe` True
    it "returns True for a Nothing value" $ do
      isWhitespaceOnly Nothing `shouldBe` True
  -- this is testing implementation, should this be tested? 
  describe "indexPage" $ do
    it "renders the index page with the correct HTML structure" $ do
      let host = "http://example.com"
          urls = [(1, "http://example.com", "short1", "2023/01/01 12:00:00")]
          renderedHtml = renderHtml $ H.html $ H.body $ do
            H.h1 "Shortener"
            H.form H.! A.method "post" H.! A.action "/" $ do
              H.input H.! A.type_ "text" H.! A.name "url"
              H.input H.! A.type_ "submit"
            H.table $ do
              H.tr $ do
                H.th "ID"
                H.th "Original"
                H.th "Short"
                H.th "Created At"
              H.tr $ do
                H.td (H.text "1")
                H.td (H.text "http://example.com")
                H.td $ H.a H.! A.href (H.toValue $ host <> "/short1") $ H.toHtml (host <> "/short1")
                H.td (H.text "2023/01/01 12:00:00")
      indexPage host urls `shouldBe` renderedHtml

  describe "getAllTables" $ do
    it "returns an empty list when the database is fresh" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      allTables <- getAllTables conn
      allTables `shouldBe` []
      close conn
    it "returns a list of tables when the database has tables" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      _ <- execute_ conn "CREATE TABLE test_table (id SERIAL PRIMARY KEY)"

      allTables <- getAllTables conn
      allTables `shouldSatisfy` (not . null)
      allTables `shouldContain` [Only "test_table"]
      close conn

  describe "applyMigrations" $ do
    it "creates the 'schema_migrations' table if it doesn't exist" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      _ <- applyMigrations conn

      allTables <- getAllTables conn
      allTables `shouldContain` [Only "schema_migrations"]
      close conn
    
    it "does not create the 'schema_migrations' table if it already exists" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      _ <- applyMigrations conn
      _ <- applyMigrations conn

      allTables <- getAllTables conn
      allTables `shouldContain` [Only "schema_migrations"]
      close conn
    
  
  describe "insertUrl" $ do
    it "inserts a URL into the database" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      _ <- applyMigrations conn

      urlId <- insertUrl conn "http://example.com"

      urlId `shouldBe` 1

      close conn
    
    it "generates a short URL for the inserted URL" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      _ <- applyMigrations conn

      urlId <- insertUrl conn "http://example.com"

      shortUrl <- query conn "SELECT id, original, short_uri, TO_CHAR(created_at, 'YYYY/MM/DD HH12:MM:SS') FROM url WHERE id = ?" (Only urlId) :: IO [Url]

      shortUrl `shouldSatisfy` (not . null)
      shortUrl `shouldSatisfy` all (\(_, _, short, _) -> T.length short == 8)

      close conn

  describe "getUrlByShortUri" $ do
    it "returns the original URL for a short URI" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase
      _ <- applyMigrations conn
      urlId <- insertUrl conn "http://example.com"
      urls <- query conn "SELECT id, original, short_uri, TO_CHAR(created_at, 'YYYY/MM/DD HH12:MM:SS') FROM url WHERE id = ?" (Only urlId) :: IO [(Int, T.Text, T.Text, T.Text)]
      let short = case urls of
            [(_, _, shortUri, _)] -> shortUri
            _ -> ""

      url <- getUrlByShortUri conn short

      url `shouldBe` Just "http://example.com"

      close conn

    it "returns Nothing for a short URI that doesn't exist" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      _ <- applyMigrations conn

      url <- getUrlByShortUri conn "short1"

      url `shouldBe` Nothing

      close conn

  describe "getUrlById" $ do
    it "returns the original URL for an ID" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase
      _ <- applyMigrations conn
      urlId <- insertUrl conn "http://example.com"

      url <- getUrlById conn urlId

      url `shouldBe` Just "http://example.com"

      close conn

    it "returns Nothing for an ID that doesn't exist" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase

      _ <- applyMigrations conn

      url <- getUrlById conn 1

      url `shouldBe` Nothing

      close conn

  describe "getAllUrls" $ do
    it "returns all URLs in the database" $ do
      _ <- resetDatabase 
      conn <- connectToTestDatabase
      _ <- applyMigrations conn
      _ <- insertUrl conn "http://example.com"
      _ <- insertUrl conn "http://example.com/path"

      urls <- getAllUrls conn

      urls `shouldSatisfy` (not . null)
      urls `shouldSatisfy` all (\(_, _, short, _) -> T.length short == 8)

      close conn
