{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TestTypes where

import qualified Data.Aeson as A
import Data.Proxy
import qualified Data.Text as T
import Elminator
import GHC.Generics

data Empty a b
    deriving Generic

instance (ToHType a, ToHType b) => ToHType (Empty a b)

data Empty2 a b
    deriving Generic

instance (ToHType a, ToHType b) => ToHType (Empty2 a b)

data SingleCon
    = SingleCon (Maybe Int) String
    deriving (Eq, Generic, Show, ToHType)

data SingleRecCon
    = SingleRecCon
    { srcF1 :: Int
    , srcF2 :: String
    }
    deriving (Eq, Generic, Show, ToHType)

data SingleConOneField
    = SingleConOneField Int
    deriving (Eq, Generic, Show, ToHType)

data SingleRecConOneField
    = SingleRecConOneField
    { srcofF1 :: Int
    }
    deriving (Eq, Generic, Show, ToHType)

data TwoCons
    = TCCon1 Int
    | TCCon2 String
    deriving (Eq, Generic, Show, ToHType)

data TwoRecCons
    = RTCCon1
        { rtcF1 :: Int
        }
    | RTCCon2
        { rtcF2 :: String
        }
    deriving (Eq, Generic, Show, ToHType)

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
        TwoCons
        SingleRecCon
    deriving (Eq, Generic, Show, ToHType)

data Comment
    = Comment
    { cContent :: String
    , cReplies :: Maybe Comment
    }
    deriving (Eq, Generic, Show, ToHType)

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
    , bcF14 :: TwoCons
    , bcF15 :: SingleRecCon
    }
    deriving (Eq, Generic, Show, ToHType)

data MixedCons
    = MxCon1
        { mxcF1 :: Int
        }
    | MxCon2 String
    deriving (Eq, Generic, Show, ToHType)

data WithSimpleMaybes
    = WithSimpleMaybesC1 (Maybe Int) (Maybe String)
    | WithSimpleMaybesC2 (Maybe String) (Maybe Int)
    deriving (Eq, Generic, Show, ToHType)

data WithMaybes
    = WithMaybes
    { mbF1 :: Maybe Int
    , mbF2 :: Maybe String
    }
    deriving (Eq, Generic, Show, ToHType)

data WithMaybesPoly a b
    = WithMaybesPoly
    { mbpF1 :: Maybe a
    , mbpF2 :: Maybe b
    }
    deriving (Eq, Generic, Show, ToHType)

data Phantom a
    = Phantom String
    deriving (Eq, Generic, Show, ToHType)

data TypeWithPhantom a
    = TypeWithPhantom (Phantom a)
    deriving (Generic, ToHType)

data RecWithList
    = RecWithList
    { rwList :: [String]
    , rwOther :: Float
    }
    deriving (Eq, Generic, Show, ToHType)

data IndRecStart
    = IndRecStart (Maybe IndRec2) Int
    deriving (Eq, Generic, Show, ToHType)

data IndRec2
    = IndRec2 IndRec3
    deriving (Eq, Generic, Show, ToHType)

data IndRec3
    = IndRec3 IndRecStart
    deriving (Eq, Generic, Show, ToHType)

newtype NTSingleCon
    = NTSingleCon
    { ntField :: Int
    }
    deriving (Eq, Generic, Show, ToHType)

newtype NTSingleCon2
    = NTSingleCon2
    { ntField2 :: Int
    }
    deriving (Eq, Generic, Show, ToHType)

newtype Tuples
    = Tuples
    { tuples :: (Int, String, Float)
    }
    deriving (Eq, Generic, Show, ToHType)

newtype NestedTuples
    = NestedTuples
    { nsttuples :: (Int, (String, Float))
    }
    deriving (Eq, Generic, Show, ToHType)

newtype NestedTuplesPoly a
    = NestedTuplesPoly
    { nsttuplespoly :: (Int, (String, a))
    }
    deriving (Generic, ToHType)

data MyExtType a b
    = MyExtType
    { extTypeField1 :: a
    , extTypeField2 :: b
    }
    deriving (Eq, Generic, Show)

newtype WithEmptyTuple a
    = WithEmptyTuple a
    deriving (Eq, Generic, Show, ToHType)

data Phantom2 a
    = Phantom2
    deriving (Eq, Generic, Show, ToHType)

newtype PhantomWrapper
    = PhantomWrapper
    { field1 :: Phantom2 PhantomWrapper
    }
    deriving (Eq, Generic, Show, ToHType)

newtype TextWraper
    = TextWraper
    { txWrapper :: T.Text
    }
    deriving (Eq, Generic, Show, ToHType)

data IndRecPolyStart a
    = IndRecPolyStart (Phantom (IndRecPoly2 a)) Int
    deriving (Generic, ToHType)

data IndRecPoly2 a
    = IndRecPoly2 (IndRecPoly3 a)
    deriving (Generic, ToHType)

data IndRecPoly3 a
    = IndRecPoly3 (IndRecPolyStart a)
    deriving (Generic, ToHType)

optionsList :: [A.Options]
optionsList = do
    flm <- [\x -> "fm" ++ x, id]
    ctm <- [\x -> "fm" ++ x, id]
    antst <- [True, False]
    onf <- [True, False]
    se <- seList
    uur <- [True, False]
    tsc <- [True, False]
    pure $
        A.defaultOptions
            { A.fieldLabelModifier = flm
            , A.constructorTagModifier = ctm
            , A.allNullaryToStringTag = antst
            , A.omitNothingFields = onf
            , A.sumEncoding = se
            , A.unwrapUnaryRecords = uur
            , A.tagSingleConstructors = tsc
            }

seList :: [A.SumEncoding]
seList =
    let taggedObjects = do
            tfn <- ["tag", "myTag"]
            cfn <- ["contents", "myContents"]
            pure $ A.TaggedObject tfn cfn
     in taggedObjects
            ++ [A.UntaggedValue, A.ObjectWithSingleField, A.TwoElemArray]

builder :: Builder
builder = do
    include (Proxy :: Proxy SingleCon) $ Everything Mono
    include (Proxy :: Proxy SingleRecCon) $ Everything Mono
    include (Proxy :: Proxy SingleConOneField) $ Everything Mono
    include (Proxy :: Proxy SingleRecConOneField) $ Everything Mono
    include (Proxy :: Proxy TwoCons) $ Everything Mono
    include (Proxy :: Proxy TwoRecCons) $ Everything Mono
    include (Proxy :: Proxy BigCon) $ Everything Mono
    include (Proxy :: Proxy BigRecCon) $ Everything Mono
    include (Proxy :: Proxy MixedCons) $ Everything Mono
    include (Proxy :: Proxy Comment) $ Everything Mono
    include (Proxy :: Proxy WithMaybes) $ Everything Mono
    include (Proxy :: Proxy WithSimpleMaybes) $ Everything Mono
    include (Proxy :: Proxy (WithMaybesPoly (Maybe String) Float)) $ Definition Poly
    include (Proxy :: Proxy (WithMaybesPoly (Maybe String) Float)) EncoderDecoder
    include (Proxy :: Proxy (Phantom ())) $ Everything Poly
    include (Proxy :: Proxy (TypeWithPhantom Float)) $ Everything Poly
    include (Proxy :: Proxy RecWithList) $ Everything Mono
    include (Proxy :: Proxy IndRecStart) $ Everything Mono
    include (Proxy :: Proxy IndRec2) $ Everything Mono
    include (Proxy :: Proxy IndRec3) $ Everything Mono
    include (Proxy :: Proxy NTSingleCon) $ Everything Mono
    include (Proxy :: Proxy NTSingleCon2) $ Everything Poly
    include (Proxy :: Proxy Tuples) $ Everything Mono
    include (Proxy :: Proxy NestedTuples) $ Everything Mono
    include (Proxy :: Proxy (NestedTuplesPoly ())) $ Definition Poly
    include (Proxy :: Proxy (WithEmptyTuple ())) $ Everything Poly
    include (Proxy :: Proxy (Phantom2 ())) $ Everything Poly
    include (Proxy :: Proxy TextWraper) $ Everything Poly
    include (Proxy :: Proxy PhantomWrapper) $ Everything Poly
    include (Proxy :: Proxy (IndRecPolyStart ())) $ Everything Poly
    include (Proxy :: Proxy (IndRecPoly2 ())) $ Everything Poly
    include (Proxy :: Proxy (IndRecPoly3 ())) $ Everything Poly
