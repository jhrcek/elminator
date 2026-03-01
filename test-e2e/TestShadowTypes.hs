{-# LANGUAGE DeriveGeneric #-}

module TestShadowTypes where

import Data.Aeson (Value)
import GHC.Generics

data BigCon
    = BigCon
        Int
        Int
        String
        String
        Float
        Int
        Int
        Int
        Float
        String
        String
        Float
        Int
        Value -- TwoCons
        Value -- SingleRecCon
    deriving Generic

data BigRecCon
    = BigRecCon
    { bcF1 :: Int
    , bcF2 :: Int
    , bcF3 :: String
    , bcF4 :: String
    , bcF5 :: Float
    , bcF6 :: Int
    , bcF7 :: Int
    , bcF8 :: Int
    , bcF9 :: Float
    , bcF10 :: String
    , bcF11 :: String
    , bcF12 :: Float
    , bcF13 :: Int
    , bcF14 :: Value -- TwoCons
    , bcF15 :: Value -- SingleRecCon
    }
    deriving Generic

data Comment
    = Comment
    { cContent :: String
    , cReplies :: Maybe Value -- Maybe Comment
    }
    deriving Generic

data IndRecStart
    = IndRecStart
        (Maybe Value) -- Maybe IndRec2
        Int
    deriving Generic

data IndRec2
    = IndRec2
        Value -- IndRec3
    deriving Generic

data IndRec3
    = IndRec3
        Value -- IndRecStart
    deriving Generic
