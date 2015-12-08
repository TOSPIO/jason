{-# LANGUAGE OverloadedStrings #-}

module Jason.Parse
       (
         parse
       ) where

import Control.Applicative
import Data.ByteString.Char8 as C8
import Data.Char
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as PC8
       (
         char8
       , notChar
       , isHorizontalSpace
       , isDigit_w8
       )
import Jason.Core (JValue (..))

bsToStr :: ByteString -> String
bsToStr = T.unpack . decodeUtf8

strToBS :: String -> ByteString
strToBS = encodeUtf8 . T.pack

nullRule :: Parser JValue
nullRule =
  string "null" >> return JNull

boolRule :: Parser JValue
boolRule =
  (string "true" >> return (JBool True)) <|>
  (string "false" >> return (JBool False))

numberRule :: Parser JValue
numberRule =
  JNumber <$> ((read . bsToStr) <$> P.takeWhile1 PC8.isDigit_w8)

stringRule :: Parser JValue
stringRule = JString <$> do
  _ <- char8 '\"'
  s <- cont
  _ <- char8 '\"'
  return $ bsToStr s
  where
    cont :: Parser ByteString
    cont = P.takeWhile (\c -> c /= fromIntegral (ord '\"'))
