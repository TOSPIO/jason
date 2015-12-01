module Jason.Core
       (
         JValue(..)
       ) where

data JValue = JNumber Double
            | JString String
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

