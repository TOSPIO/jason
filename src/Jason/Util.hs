module Jason.Util
       (
         bsToStr
       , strToBS
       ) where

import Data.ByteString
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

bsToStr :: ByteString -> String
bsToStr = T.unpack . decodeUtf8

strToBS :: String -> ByteString
strToBS = encodeUtf8 . T.pack
