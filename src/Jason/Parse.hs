{-# LANGUAGE OverloadedStrings #-}

module Jason.Parse
       (
         parse
       ) where

import Control.Applicative
import Data.ByteString.Char8 as C8
import Data.Char
import Data.Attoparsec.ByteString as P hiding (parse)
import qualified Data.Attoparsec.ByteString as P (parse)
import Data.Attoparsec.ByteString.Char8 as PC8 hiding (parse)
import Jason.Core (JValue (..))
import Jason.Util (bsToStr)

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
      (
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
        )
      ) <|> notChar '\"'


jRule :: Parser JValue
jRule =
  skipSpace *>
  (
    jNullRule <|>
    jBoolRule <|>
    jNumberRule <|>
    jStringRule <|>
    jArrayRule <|>
    jObjectRule
  ) <* skipSpace

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
    char '[' *> skipSpace *> sepBy' jRule (skipSpace *> char ',' <* skipSpace) <* skipSpace <* char ']'
  )

jObjectRule :: Parser JValue
jObjectRule =
  JObject <$>
  (
    char '{' *> skipSpace *> sepBy' kvp (skipSpace *> char ',' <* skipSpace) <* skipSpace <* char '}'
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


parseResult :: ByteString -> Result JValue
parseResult bs = P.feed (P.parse jRule bs) ""

parse :: ByteString -> Maybe JValue
parse = maybeResult . parseResult
