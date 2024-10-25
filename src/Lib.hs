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

data Speech
  = Speech (T.Text, T.Text) -- (Speaker, Content)
  | NullSpeech
  deriving (Show)

data ConversationTree
  = ConversationBranch Speech [ConversationTree]
  | ConversationEnd
  deriving (Show)

data Conversation = Conversation
  { conversationTitle :: T.Text,
    conversationContent :: ConversationTree
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
    isParentObject (Object y) = fromMaybe True $ do
      _ <- y !? "parent"
      return False
    isParentObject _ = False

    -- Returns True if and only if it has any message conent
    hasContent :: Object -> Bool
    hasContent y = fromMaybe False $ do
      _ <- y !? "message"
      return True

    conversationStartingFrom :: Key -> Object -> Either String ConversationTree
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
            then return $ ConversationBranch (Speech (role, say)) [ConversationEnd]
            else do
              processedChildren <-
                sequence $
                  V.toList $
                    V.map
                      ( \k' -> do
                          k <- toString' k'
                          conversationStartingFrom (fromText k) mapping
                      )
                      children
              return $ ConversationBranch (Speech (role, say)) processedChildren
        else
          if V.null children
            then return ConversationEnd
            else do
              processedChildren <-
                sequence $
                  V.toList $
                    V.map
                      ( \k' -> do
                          k <- toString' k'
                          conversationStartingFrom (fromText k) mapping
                      )
                      children
              return $ ConversationBranch NullSpeech processedChildren

-- Todo: Print all the branches
printConversation :: Conversation -> IO ()
printConversation (Conversation title content) = do
  putStrLn $ "<!-- " ++ T.unpack title ++ " -->"
  printSpeech content
  where
    printSpeech ConversationEnd = return ()
    printSpeech (ConversationBranch (Speech (speaker, say)) (y : ys)) = do
      putStr $ "**" ++ T.unpack speaker ++ ":** "
      putStrLn $ T.unpack say ++ "\n"
      printSpeech y
    printSpeech (ConversationBranch NullSpeech (y : ys)) = printSpeech y
    printSpeech _ = return ()

someFunc :: IO ()
someFunc = do
  argv <- getArgs
  if null argv
    then
      putStrLn "The first argument must be a URL"
    else do
      test <- scrapeRawJSON $ head argv
      case test of
        Just x -> case getConversation $ convertString x of
          Left err -> putStrLn $ "Err: " ++ err
          Right conv -> printConversation conv
        Nothing -> putStrLn "failed"
