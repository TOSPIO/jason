module Jason.Parse
       (
         parse
       ) where

import qualified Text.ParserCombinators.Parsec as P
import Jason.Core

numberRule :: P.Parser JValue
numberRule = do
  s <- P.many1 P.digit
  return $ JNumber $ read s

jsonChar :: P.Parser Char
jsonChar = do
  

jsonString :: P.Parser String
jsonString = do
  P.char '\"'
  manyTill jsonChar '\"'
  

stringRule :: P.Parser JValue
stringRule = do
  P.char '\"'
  P.try
  P.char '\"'

parse :: String -> JValue
parse = undefined
