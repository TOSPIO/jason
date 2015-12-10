{-# LANGUAGE OverloadedStrings #-}

module Jason.Parse
       (
         parse
       ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString       as P hiding (parse)
import qualified Data.Attoparsec.ByteString       as P (parse)
import           Data.Attoparsec.ByteString.Char8 as PC8 hiding (parse)
import           Data.ByteString                  as BS
import           Data.ByteString.Char8            as C8
import           Data.Char
import           Data.Text                        as T
import           Jason.Core                       (JValue (..))
import           Jason.Util                       (bsToText, strToBS)

stringRule :: Parser Text
stringRule =
  char '\"' *> (bsToText . C8.concat <$> many' (contNoEscape <|> contEscape)) <* char '\"'
  where
    contNoEscape :: Parser ByteString
    contNoEscape = PC8.takeWhile1 (\c -> c /= '\\' && c /= '\"')
    contEscape :: Parser ByteString
    contEscape =
      char '\\' *>
      (
        (char '\"' >> return "\"") <|>
        (char '\\' >> return "\\") <|>
        (char '/' >> return "/") <|>
        (char 'b' >> return "\b") <|>
        (char 'f' >> return "\f") <|>
        (char 'n' >> return "\n") <|>
        (char 'r' >> return "\r") <|>
        (char 't' >> return "\t") <|>
        liftA (strToBS . (:[]) . chr . read) (char 'u' *> PC8.count 4 PC8.digit)
      )


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
  "null" >> return JNull

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
    char8 '[' *> skipSpace *> sepBy' jRule (skipSpace *> char8 ',' <* skipSpace) <* skipSpace <* char8 ']'
  )

jObjectRule :: Parser JValue
jObjectRule =
  JObject <$>
  (
    char8 '{' *> skipSpace *> sepBy' kvp (skipSpace *> char8 ',' <* skipSpace) <* skipSpace <* char8 '}'
  )
  where
    kvp :: Parser (Text, JValue)
    kvp =
      (,) <$> (stringRule <* skipSpace <* char8 ':') <*> (skipSpace *> jRule)


parseResult :: ByteString -> Result JValue
parseResult bs = P.feed (P.parse jRule bs) BS.empty

parse :: ByteString -> Maybe JValue
parse = maybeResult . parseResult
