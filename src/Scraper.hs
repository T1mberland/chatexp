{-# LANGUAGE OverloadedStrings #-}

module Scraper (scrapeRawJSON) where

import qualified Data.Text as T
import Text.HTML.Scalpel

scrapeRawJSON :: String -> IO (Maybe T.Text)
scrapeRawJSON url = do
  scraped <- scrapeURL url foo
  case scraped of
    Just (_ : x : _) -> return $ extractJSON x -- get the second element, which is the second <script> content from the top.
    _ -> return Nothing
  where
    foo :: Scraper T.Text [T.Text]
    foo = texts "script" -- get the content inside of <script> tags.
    extractJSON :: T.Text -> Maybe T.Text
    extractJSON str = do
      (js, _) <- (T.unsnoc . T.strip . T.replace "=" "" . T.replace "window.__remixContext" "") str
      return js
