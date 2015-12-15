{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}

module Jason.HandRoll.Parse
       (
         parse
       ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.State.Lazy
import           Control.Monad.Writer.Lazy as WL
import           Data.Attoparsec.ByteString       as APBS
import           Data.Attoparsec.ByteString.Char8 as APC8
import           Data.ByteString                  as BS
import           Data.ByteString.Char8            as C8
import           Data.Char
import           Data.Maybe
import           Data.Text                        as T
import           Data.Text.Encoding               as E (decodeUtf8, encodeUtf8)
import           Data.Word                        as W
import           Jason.Core                       (JValue (..))
import qualified Numeric                          as N (readHex)

data Token = TChar Char
           | TStr Text
           | TNumber Double
           | TLObj
           | TRObj
           | TLArr
           | TRArr
           | TComma
           | TBool Bool
           | TNull
           | TError Pos String

type Pos = Int
type TokenTest = WL.WriterT [Token] (State ByteString) ()

readHex :: ByteString -> Maybe Int
readHex bs = let
  parseResults = N.readHex . C8.unpack $ bs
  in
    case parseResults of
    [] -> Nothing
    a:_ -> Just $ fst a

maybeTError :: Maybe a -> TokenTest -> TokenTest
maybeTError Nothing _ = put BS.empty >> tell [TError 0 "Error"]
maybeTError (Just _) t = t

scanEscapeChar :: TokenTest
scanEscapeChar = do
  bs <- get
  maybeTError C8.un
  case C8.uncons bs of
    '\"' -> put bss >> tell [TChar '\"']
    '\\' -> put bss >> tell [TChar '\\']
    '/' -> put bss >> tell [TChar '/']
    'b' -> put bss >> tell [TChar '\b']
    'f' -> put bss >> tell [TChar '\f']
    'n' -> put bss >> tell [TChar '\n']
    'r' -> put bss >> tell [TChar '\r']
    'u' -> scanUnicodeChar
  where
    scanUnicodeChar :: TokenTest
    scanUnicodeChar = do
      bs <- get
      let (d4, bss) = BS.splitAt 4 bs
      hex <- readHex d4
      put bss
      tell [TChar (chr hex)]

scanString :: TokenTest
scanString bs = do
  pos <- findEnclosing bs 0
  let (t, bss) = C8.splitAt pos bs
  return (TStr . decodeUtf8 $ t, bss)
  where
    findEnclosing :: ByteString -> Int -> Maybe Int
    findEnclosing bs sp = do
      guard $ sp < C8.length bs - 1
      let c = bs `C8.index` sp
      if
        | c == '\"' -> Just sp
        | c == '\\' -> findEnclosing bs (sp+2)
        | otherwise -> Nothing

scanNull :: TokenTest
scanNull bs = case C8.splitAt 3 bs of
  ("ull", bss) -> put bss >> tell TNull
  _ -> put bs

scanTrue :: TokenTest
scanTrue bs = case C8.splitAt 3 bs of
  ("rue", bss) -> put bss >> tell (TBool True)
  _ -> put bs

scanFalse :: TokenTest
scanFalse bs = case C8.splitAt 4 bs of
  ("alse", bss) -> put bss >> tell (TBool False)
  _ -> put bs

scanNumber :: TokenTest
scanNumber bs =
  case flip APBS.feed "" . APBS.parse APC8.scientific $ bs of
    Done bss r -> put bss >> tell (TNumber r)
    _ -> put bs

scanSelect :: TokenTest
scanSelect bs = case C8.uncons bs of
  Nothing -> return ()
  Just (h, bss) -> if
    h `BS.elem` " \r\n\t"
    then scanSelect bss
    else case h of
    Just (h, bss) -> case h of
      '\\' -> scanEscapeChar bss
      '\"' -> scanString bss
      '{' -> put bss >> tell TLObj
      '[' -> put bss >> tell TLArr
      'n' -> scanNull bss
      't' -> scanTrue bss
      'f' -> scanFalse bss
      _ -> scanNumber bs

parse :: [Token] -> JValue
parse = undefined
