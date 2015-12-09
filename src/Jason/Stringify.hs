{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Jason.Stringify
       (
         stringifyToBuilder
       , stringify
       ) where

import Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder as BSB
import Data.Char
import Data.Monoid
import Jason.Core
       (
         JValue(..)
       )
import Jason.Util (strToBS)

stringifyToBuilder :: JValue -> Builder
stringifyToBuilder JNull = "null"
stringifyToBuilder (JNumber val) = doubleDec val
stringifyToBuilder (JString val) =
  charUtf8 '\"' <> mconcat (fmap jCharToBuilder val) <> charUtf8 '\"' where
  jCharToBuilder :: Char -> Builder
  jCharToBuilder c =
      case c of
      '\"' -> "\\\""
      '\\' -> "\\\\"
      '/' -> "\\/"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      (isAscii -> True) -> charUtf8 c
      _ -> "\\u" <> intDec (ord c)
stringifyToBuilder (JBool val) = if val then "true" else "false"
stringifyToBuilder (JArray val) =
  charUtf8 '[' <>
  byteString
  (
    intercalate ", " (fmap (toStrict . toLazyByteString . stringifyToBuilder) val)
  ) <>
  charUtf8 ']'
stringifyToBuilder (JObject val) =
  charUtf8 '{' <>
  byteString
  (
    intercalate ", "
    (
      fmap (\(k, v) ->
             strToBS k <> ": " <>
             (toStrict . toLazyByteString) (stringifyToBuilder v)
           ) val
    )
  ) <>
  charUtf8 '}'

stringify :: JValue -> ByteString
stringify = toStrict . toLazyByteString . stringifyToBuilder
