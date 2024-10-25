{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.KeyMap (toAscList, (!?))
import qualified Data.ByteString.Lazy as B
import Data.Either.Extra
import Data.Maybe
import Data.String.Conversions
import qualified Data.Text as T
import qualified Data.Vector as V
import Safe
import System.Environment (getArgs)
import Text.HTML.Scalpel

data Conversation = Conversation
  { conversationTitle :: T.Text,
    conversationContent :: [(T.Text, T.Text)] -- [(Speaker, Content)]
  }
  deriving (Show)

scrapeRawJSON :: String -> IO (Maybe T.Text)
scrapeRawJSON url = do
  scraped <- scrapeURL url foo
  case scraped of
    Just (_ : x : _) -> return $ extractJSON x -- get the second element, which is the second <script> content from the top.
    _ -> return Nothing
  where
    foo :: Scraper T.Text [T.Text]
    foo = texts "script" -- get the contents inside of <script> tags.
    extractJSON :: T.Text -> Maybe T.Text
    extractJSON str = do
      (js, _) <- (T.unsnoc . T.strip . T.replace "=" "" . T.replace "window.__remixContext" "") str
      return js

-- Maybe monad version
getConversation' :: B.ByteString -> Maybe Conversation
getConversation' x = do
  y <- decode x :: Maybe Object
  (Object state) <- y !? "state"
  (Object loaderData) <- state !? "loaderData"
  (Object routes) <- loaderData !? "routes/share.$shareId.($action)"
  (Object serverResp) <- routes !? "serverResponse"
  (Object dat) <- serverResp !? "data"

  (String title) <- dat !? "title"
  (Object mapping) <- dat !? "mapping"

  parentKey <- fst <$> (headMay . filter (isParentObject . snd) . toAscList) mapping
  convContent <- conversationStartingFrom parentKey mapping
  return $ Conversation title convContent
  where
    -- Returns True if and only if it does not have a parent object.
    isParentObject :: Value -> Bool
    isParentObject (Object y) =
      fromMaybe True $ do
        _ <- y !? "parent"
        return False
    isParentObject _ = False

    hasContent :: Object -> Bool
    hasContent y = fromMaybe False $ do
      _ <- y !? "parent"
      return True

    conversationStartingFrom :: Key -> Object -> Maybe [(T.Text, T.Text)]
    conversationStartingFrom key mapping = do
      (Object speech) <- mapping !? key
      (Array children) <- speech !? "children"
      if hasContent speech
        then do
          (Object message) <- speech !? "message"

          (Object author) <- message !? "author"
          (String role) <- author !? "role"

          (Object content) <- message !? "content"
          (Array parts) <- content !? "parts"

          (String say) <- parts V.!? 0

          if V.null children
            then return [(role, say)]
            else do
              (String firstChild) <- children V.!? 0
              next <- conversationStartingFrom (fromText firstChild) mapping
              return ((role, say) : next)
        else
          if V.null children
            then return []
            else do
              (String firstChild) <- children V.!? 0
              conversationStartingFrom (fromText firstChild) mapping

getConversation :: B.ByteString -> Either String Conversation
getConversation x = do
  parsedJSON <- maybeToEither ("Failed to parse the following JSON (possibly reading content from a different <script>):\n----------\n" ++ convertString x) (decode x :: Maybe Object)
  state <- toObject' =<< (parsedJSON ? "state")
  loaderData <- toObject' =<< (state ? "loaderData")
  routes <- toObject' =<< (loaderData ? "routes/share.$shareId.($action)")
  serverResp <- toObject' =<< (routes ? "serverResponse")
  dat <- toObject' =<< (serverResp ? "data")

  title <- toString' =<< (dat ? "title")
  mapping <- toObject' =<< (dat ? "mapping")

  parentKey <-
    fst
      <$> ( maybeToEither "Failed to find the beginning of conversation."
              . headMay
              . filter (isParentObject . snd)
              . toAscList
          )
        mapping
  convContent <- conversationStartingFrom parentKey mapping
  return $ Conversation title convContent
  where
    (?) keymap' key' = maybeToEither ("Missing key: " ++ toString key') (keymap' !? key')

    toObject' :: Value -> Either String Object
    toObject' (Object obj) = Right obj
    toObject' (Array arr) = Left ("Expected an `Object`, but found an `Array` with value: " ++ show arr)
    toObject' (String str) = Left ("Expected an `Object`, but found a `String` with value: " ++ show str)
    toObject' (Number num) = Left ("Expected an `Object`, but found a `Number` with value: " ++ show num)
    toObject' (Bool bool) = Left ("Expected an `Object`, but found a `Bool` with value: " ++ show bool)
    toObject' Null = Left "Expected an `Object`, but found `Null`."

    toString' :: Value -> Either String T.Text
    toString' (String str) = Right str
    toString' (Object obj) = Left ("Expected a `String`, but found an `Object` with value: " ++ show obj)
    toString' (Array arr) = Left ("Expected a `String`, but found an `Array` with value: " ++ show arr)
    toString' (Number num) = Left ("Expected a `String`, but found a `Number` with value: " ++ show num)
    toString' (Bool bool) = Left ("Expected a `String`, but found a `Bool` with value: " ++ show bool)
    toString' Null = Left "Expected a `String`, but found `Null`."

    toArray' :: Value -> Either String Array
    toArray' (Array arr) = Right arr
    toArray' (Object obj) = Left ("Expected an `Array`, but found an `Object` with value: " ++ show obj)
    toArray' (String str) = Left ("Expected an `Array`, but found a `String` with value: " ++ show str)
    toArray' (Number num) = Left ("Expected an `Array`, but found a `Number` with value: " ++ show num)
    toArray' (Bool bool) = Left ("Expected an `Array`, but found a `Bool` with value: " ++ show bool)
    toArray' Null = Left "Expected an `Array`, but found `Null`."

    -- Returns True if and only if it does not have a parent object.
    isParentObject :: Value -> Bool
    isParentObject (Object y) =
      fromMaybe True $ do
        _ <- y !? "parent"
        return False
    isParentObject _ = False

    -- Returns True if and only if it has any message conent
    hasContent :: Object -> Bool
    hasContent y = fromMaybe False $ do
      _ <- y !? "message"
      return True

    conversationStartingFrom :: Key -> Object -> Either String [(T.Text, T.Text)]
    conversationStartingFrom key mapping = do
      speech <- toObject' =<< mapping ? key
      children <- toArray' =<< speech ? "children"
      if hasContent speech
        then do
          message <- toObject' =<< speech ? "message"

          author <- toObject' =<< message ? "author"
          role <- toString' =<< author ? "role"

          content <- toObject' =<< message ? "content"
          parts <- toArray' =<< content ? "parts"
          say <- toString' =<< maybeToEither "`parts` is empty." (parts V.!? 0)

          if V.null children
            then return [(role, say)]
            else do
              firstChild <- toString' =<< maybeToEither "Err(0): This error should not happen!" (children V.!? 0)
              next <- conversationStartingFrom (fromText firstChild) mapping
              return ((role, say) : next)
        else
          if V.null children
            then return []
            else do
              firstChild <- toString' =<< maybeToEither "Err(1): This error should not happen!" (children V.!? 0)
              conversationStartingFrom (fromText firstChild) mapping

someFunc :: IO ()
someFunc = do
  argv <- getArgs
  if null argv
    then
      putStrLn "The first argument must be an URL"
    else do
      test <- scrapeRawJSON $ head argv
      case test of
        Just x -> print $ getConversation $ convertString x
        Nothing -> putStrLn "failed"
