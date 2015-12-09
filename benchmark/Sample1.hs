module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.ByteString as BS
import Jason
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let times = read (Prelude.head args) :: Int
  bs <- BS.readFile "../fixtures/sample1.json"
  replicateM_ times $ do
    let (Just jVal) = parse bs
    void $ evaluate $ force jVal
