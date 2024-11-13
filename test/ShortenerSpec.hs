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

