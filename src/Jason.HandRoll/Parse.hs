{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Jason.HandRoll.Parse
       (
         parse
       ) where

import Data.ByteString as BS
import Data.Text as T
import Data.Word as W
import Control.Exception

class Token a

newtype TChar = TChar Word8
instance Token TChar

newtype TString = TString ByteString
instance Token TString

type Pos = Int
type TokenTest = (Token t) => ByteString -> Maybe (t, ByteString)

scanEscapeChar :: TokenTest
scanEscapeChar bs =
  BS.uncons bs >> \(h, bsx) ->
  case h of
  '\"' -> return (TChar '\"', bsx)
  '\\' -> return (TChar '\\', bsx)
  '/' -> return (TChar '/', bsx)
  'b' -> return (TChar '\b', bsx)
  'f' -> return (TChar '\f', bsx)
  'n' -> return (TChar '\n', bsx)
  'r' -> return (TChar '\r', bsx)
  'u' -> scanUnicodeChar bsx
  where
    scanUnicodeChar :: TokenTest
    scanUnicodeChar bs = let
      (d4, bsx) = BS.splitAt 4 bs
      in
        _

scanString :: TokenTest
scanString = undefined

scanObject :: TokenTest
scanObject = undefined

scanArray :: TokenTest
scanArray = undefined

scanNull :: TokenTest
scanNull = undefined

scanTrue :: TokenTest
scanTrue = undefined

scanFalse :: TokenTest
scanFalse = undefined

scanNumber :: TokenTest
scanNumber = undefined

tokenize :: ByteString -> Pos -> Maybe (Token, ByteString)
tokenize bs pos = let
  x = BS.uncons bs
  in
    case x of
    Nothing -> Nothing
    Just (h, bsx) -> noname h bsx
  where
    noname :: ByteString -> Word8 -> ByteString -> Maybe (Token, ByteString)
    noname bs w bsx =
      case w of
      '\\' -> scanEscapeChar bsx
      '\"' -> scanString bsx
      '{' -> scanObject bsx
      '[' -> scanArray bsx
      'n' -> scanNull bsx
      't' -> scanTrue bsx
      'f' -> scanFalse bsx
      d -> scanNumber bs
