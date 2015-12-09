{-# LANGUAGE OverloadedStrings #-}

module Jason.Parse
       (
         parse
       ) where

import Control.Applicative
import Data.ByteString.Char8 as C8
import Data.Char
import Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.ByteString as P hiding (parse)
import qualified Data.Attoparsec.ByteString as P (parse)
import Data.Attoparsec.ByteString.Char8 as PC8 hiding (parse)
import Jason.Core (JValue (..))

bsToStr :: ByteString -> String
bsToStr = T.unpack . decodeUtf8

stringRule :: Parser String
stringRule =
  bsToStr <$>
  (
    char '\"' *> cont <* char '\"'
  )
  where
    cont :: Parser ByteString
    cont = C8.pack <$> many' jChar
    jChar :: Parser Char
    jChar =
      char '\\' *>
      (
        (char '\"' >> return '\"') <|>
        (char '\\' >> return '\\') <|>
        (char '/' >> return '/') <|>
        (char 'b' >> return '\b') <|>
        (char 'f' >> return '\f') <|>
        (char 'n' >> return '\n') <|>
        (char 'r' >> return '\r') <|>
        (char 't' >> return '\t') <|>
        liftA (chr . read) (char 'u' *> PC8.count 4 PC8.digit)
      ) <|>
      notChar '\"'


jRule :: Parser JValue
jRule =
  jNullRule <|>
  jBoolRule <|>
  jNumberRule <|>
  jStringRule <|>
  jArrayRule <|>
  jObjectRule

jNullRule :: Parser JValue
jNullRule =
  string "null" >> return JNull

jBoolRule :: Parser JValue
jBoolRule =
  ("true" >> return (JBool True)) <|>
  ("false" >> return (JBool False))

jNumberRule :: Parser JValue
jNumberRule =
  JNumber <$> double

jStringRule :: Parser JValue
jStringRule =
  JString <$> stringRule

jArrayRule :: Parser JValue
jArrayRule =
  JArray <$>
  (
    char '[' *> sepBy' jRule (skipSpace *> char ',' <* skipSpace) <* char ']'
  )

jObjectRule :: Parser JValue
jObjectRule =
  JObject <$>
  (
    char '{' *> sepBy' kvp (skipSpace *> char ',' <* skipSpace) <* char '}'
  )
  where
    kvp :: Parser (String, JValue)
    kvp = do
      key <- stringRule
      _ <- skipSpace
      _ <- char ':'
      _ <- skipSpace
      val <- jRule
      return (key, val)

parse :: ByteString -> Result JValue
parse = P.parse jRule
