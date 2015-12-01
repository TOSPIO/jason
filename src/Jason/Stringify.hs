{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jason.Stringify
       (
         stringify
       ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Jason.Core (
  JValue(..)
  )

type JStringTransformer a = WriterT String (Reader String) a

-- 写了还不如不写
jStrTransform :: JStringTransformer ()
jStrTransform = do
  s <- ask
  case s of
    [] -> return ()
    (c:cs) -> tell (escape c) >> local (const cs) jStrTransform
  where
    escape :: Char -> String
    escape '\t' = "\\t"
    escape '\\' = "\\\\"
    escape '/' = "\\/"
    escape '\r' = "\\r"
    escape '\n' = "\\n"
    escape '\f' = "\\f"
    escape '\b' = "\\b"
    escape '"' = "\\\""
    -- TODO: \uXXXX
    escape a = [a]

-- 我到底在干几把毛
jStr :: String -> String
jStr = runReader (execWriterT jStrTransform)

stringify :: JValue -> String
stringify (JNumber val) = show val
stringify (JString val) = "\"" ++ val ++ "\""
stringify (JBool val) = if val then "true" else "false"
stringify JNull = "null"
stringify (JObject val) = "{" ++ intercalate ", " (fmap (\(k, v) -> k ++ ": " ++ stringify v) val) ++ "}"
stringify (JArray val) = "[" ++ intercalate ", " (fmap stringify val) ++ "]"
