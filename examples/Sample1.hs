{-# LANGUAGE NoImplicitPrelude #-}

module Sample1 where

import qualified Data.ByteString as BS
import Prelude (($))
import System.IO
import Jason (parse)


main :: IO ()
main = do
  bs <- BS.readFile "json_samples/sample1.json"
  print $ parse bs
