{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Jason.Stringify
       (
         stringifyToText
       , stringify
       ) where

import           Data.ByteString         as BS
import           Data.ByteString.Builder as BSB
import           Data.ByteString.Lazy    (toStrict)
import           Data.Char
import           Data.Monoid
import           Data.Text               as T
import           Jason.Core              (JValue (..))
import           Jason.Util              (textToBS)

stringifyToText :: JValue -> Text
stringifyToText JNull = "null"
stringifyToText (JNumber val) = T.pack $ show val
stringifyToText (JString val) =
  ('\"' `T.cons` T.concatMap jCharToText val) `T.snoc` '\"' where
  jCharToText :: Char -> Text
  jCharToText c =
      case c of
      '\"' -> "\\\""
      '\\' -> "\\\\"
      '/' -> "\\/"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      (isAscii -> True) -> T.singleton c
      _ -> "\\u" `T.append` T.pack (show (ord c))
stringifyToText (JBool val) = if val then "true" else "false"
stringifyToText (JArray val) =
  '[' `T.cons`
    T.intercalate ", " (Prelude.map stringifyToText val)
  `T.snoc` ']'
stringifyToText (JObject val) =
  '{' `T.cons`
  T.intercalate ", "
  (
    fmap (\(k, v) ->
           k `T.append` ": " `T.append`
           stringifyToText v
         ) val
  )
  `T.snoc` '}'

stringify :: JValue -> ByteString
stringify = textToBS . stringifyToText
