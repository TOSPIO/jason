{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jason.Core
       (
         JValue (..)
       ) where

import GHC.Generics (Generic)
import Control.DeepSeq

data JValue = JNull
            | JBool !Bool
            | JNumber !Double
            | JString !String
            | JArray ![JValue]
            | JObject ![(String, JValue)]
            deriving (Eq, Ord, Show, Generic, NFData)

