{-# LANGUAGE NoImplicitPrelude #-}

module Sample1 where

import qualified Data.ByteString as BS
import Prelude (($))
import System.IO
import Jason (parse)


main :: IO ()
main = do
  bs <- BS.readFile "../fixtures/sample1.json"
  print $ parse bs
