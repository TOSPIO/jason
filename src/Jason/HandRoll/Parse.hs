{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE MultiWayIf #-}

module Jason.HandRoll.Parse
       (
         parse
       ) where

import           Control.Exception
import           Control.Monad
import           Data.ByteString       as BS
import           Data.ByteString.Char8 as C8
import           Data.Text             as T
import           Data.Text.Encoding    as E
import           Data.Word             as W

class Token t where
  text :: t -> Text

instance Token Char where
  text = T.singleton

instance Token ByteString where
  text = decodeUtf8

data AnyToken = forall t. (Token t) => MkAT { unToken :: t }
instance Token AnyToken where
  text (MkAT t) = text t

mkAT :: (Token t) => t -> AnyToken
mkAT = MkAT

type Pos = Int
type TokenTest = ByteString -> Maybe (AnyToken, ByteString)

scanEscapeChar :: TokenTest
scanEscapeChar bs =
  C8.uncons bs >>= \(h, bsx) ->
  case h of
  '\"' -> return (mkAT '\"', bsx)
  '\\' -> return (mkAT '\\', bsx)
  '/' -> return (mkAT '/', bsx)
  'b' -> return (mkAT '\b', bsx)
  'f' -> return (mkAT '\f', bsx)
  'n' -> return (mkAT '\n', bsx)
  'r' -> return (mkAT '\r', bsx)
  'u' -> scanUnicodeChar bsx
  where
    scanUnicodeChar :: TokenTest
    scanUnicodeChar bs = let
      (d4, bsx) = BS.splitAt 4 bs
      in
        undefined

scanString :: TokenTest
scanString bs = do
  pos <- findEnclosing bs 0
  let (t, bsx) = C8.splitAt pos bs
  return (mkAT t, bsx)
  where
    findEnclosing :: ByteString -> Int -> Maybe Int
    findEnclosing bs sp = do
      guard $ sp < C8.length bs - 1
      let c = bs `C8.index` sp
      if
        | c == '\"' -> return sp
        | c == '\\' -> findEnclosing bs (sp+2)
        | otherwise -> Nothing

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

tokenize :: TokenTest
tokenize bs = let
  x = C8.uncons bs
  in
    case x of
    Nothing -> Nothing
    Just (h, bsx) -> noname bs h bsx
  where
    noname :: (Token a) => ByteString -> Char -> ByteString -> Maybe (a, ByteString)
    noname bs h bsx =
      case h of
      '\\' -> scanEscapeChar bsx
      '\"' -> scanString bsx
      '{' -> scanObject bsx
      '[' -> scanArray bsx
      'n' -> scanNull bsx
      't' -> scanTrue bsx
      'f' -> scanFalse bsx
      d -> scanNumber bs
