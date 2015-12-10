module Jason.Util
       (
         bsToStr
       , strToBS
       , bsToText
       , textToBS
       ) where

import Data.ByteString
import Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

bsToStr :: ByteString -> String
bsToStr = T.unpack . decodeUtf8

strToBS :: String -> ByteString
strToBS = encodeUtf8 . T.pack

bsToText :: ByteString -> Text
bsToText = decodeUtf8

textToBS :: Text -> ByteString
textToBS = encodeUtf8
