module Jason.Core
       (
         JValue (..)
       ) where

data JValue = JNull
            | JBool Bool
            | JNumber Double
            | JString String
            | JArray [JValue]
            | JObject [(String, JValue)]
            deriving (Eq, Ord, Show)

