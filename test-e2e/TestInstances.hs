{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestInstances where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.TypeLits
import qualified TestShadowTypes as Shadow
import qualified TestTypes
import TestWrapper

-- BigCon: fields 14,15 are TwoCons, SingleRecCon

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    ToJSON (ToFromJsonWithOptions n TestTypes.BigCon)
    where
    toJSON (ToFromJsonWithOptions (TestTypes.BigCon f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15)) =
        genericToJSON (getOptions @n) $
            Shadow.BigCon f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13
                (toJSON (ToFromJsonWithOptions @n f14))
                (toJSON (ToFromJsonWithOptions @n f15))

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    FromJSON (ToFromJsonWithOptions n TestTypes.BigCon)
    where
    parseJSON v = do
        Shadow.BigCon f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14v f15v <-
            genericParseJSON (getOptions @n) v
        f14 <- unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.TwoCons) f14v
        f15 <- unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.SingleRecCon) f15v
        pure $ ToFromJsonWithOptions $ TestTypes.BigCon f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15

-- BigRecCon: fields bcF14, bcF15 are TwoCons, SingleRecCon

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    ToJSON (ToFromJsonWithOptions n TestTypes.BigRecCon)
    where
    toJSON (ToFromJsonWithOptions brc) =
        genericToJSON (getOptions @n) $
            Shadow.BigRecCon
                { Shadow.bcF1 = TestTypes.bcF1 brc
                , Shadow.bcF2 = TestTypes.bcF2 brc
                , Shadow.bcF3 = TestTypes.bcF3 brc
                , Shadow.bcF4 = TestTypes.bcF4 brc
                , Shadow.bcF5 = TestTypes.bcF5 brc
                , Shadow.bcF6 = TestTypes.bcF6 brc
                , Shadow.bcF7 = TestTypes.bcF7 brc
                , Shadow.bcF8 = TestTypes.bcF8 brc
                , Shadow.bcF9 = TestTypes.bcF9 brc
                , Shadow.bcF10 = TestTypes.bcF10 brc
                , Shadow.bcF11 = TestTypes.bcF11 brc
                , Shadow.bcF12 = TestTypes.bcF12 brc
                , Shadow.bcF13 = TestTypes.bcF13 brc
                , Shadow.bcF14 = toJSON (ToFromJsonWithOptions @n (TestTypes.bcF14 brc))
                , Shadow.bcF15 = toJSON (ToFromJsonWithOptions @n (TestTypes.bcF15 brc))
                }

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    FromJSON (ToFromJsonWithOptions n TestTypes.BigRecCon)
    where
    parseJSON v = do
        s <- genericParseJSON (getOptions @n) v :: Parser Shadow.BigRecCon
        f14 <- unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.TwoCons) (Shadow.bcF14 s)
        f15 <- unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.SingleRecCon) (Shadow.bcF15 s)
        pure $
            ToFromJsonWithOptions $
                TestTypes.BigRecCon
                    { TestTypes.bcF1 = Shadow.bcF1 s
                    , TestTypes.bcF2 = Shadow.bcF2 s
                    , TestTypes.bcF3 = Shadow.bcF3 s
                    , TestTypes.bcF4 = Shadow.bcF4 s
                    , TestTypes.bcF5 = Shadow.bcF5 s
                    , TestTypes.bcF6 = Shadow.bcF6 s
                    , TestTypes.bcF7 = Shadow.bcF7 s
                    , TestTypes.bcF8 = Shadow.bcF8 s
                    , TestTypes.bcF9 = Shadow.bcF9 s
                    , TestTypes.bcF10 = Shadow.bcF10 s
                    , TestTypes.bcF11 = Shadow.bcF11 s
                    , TestTypes.bcF12 = Shadow.bcF12 s
                    , TestTypes.bcF13 = Shadow.bcF13 s
                    , TestTypes.bcF14 = f14
                    , TestTypes.bcF15 = f15
                    }

-- Comment: recursive (cReplies :: Maybe Comment)

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    ToJSON (ToFromJsonWithOptions n TestTypes.Comment)
    where
    toJSON (ToFromJsonWithOptions c) =
        genericToJSON (getOptions @n) $
            Shadow.Comment
                { Shadow.cContent = TestTypes.cContent c
                , Shadow.cReplies = fmap (\r -> toJSON (ToFromJsonWithOptions @n r)) (TestTypes.cReplies c)
                }

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    FromJSON (ToFromJsonWithOptions n TestTypes.Comment)
    where
    parseJSON v = do
        s <- genericParseJSON (getOptions @n) v :: Parser Shadow.Comment
        replies <- case Shadow.cReplies s of
            Nothing -> pure Nothing
            Just rv -> Just . unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.Comment) rv
        pure $
            ToFromJsonWithOptions $
                TestTypes.Comment
                    { TestTypes.cContent = Shadow.cContent s
                    , TestTypes.cReplies = replies
                    }

-- IndRecStart: Maybe IndRec2

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    ToJSON (ToFromJsonWithOptions n TestTypes.IndRecStart)
    where
    toJSON (ToFromJsonWithOptions (TestTypes.IndRecStart mf2 i)) =
        genericToJSON (getOptions @n) $
            Shadow.IndRecStart
                (fmap (\x -> toJSON (ToFromJsonWithOptions @n x)) mf2)
                i

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    FromJSON (ToFromJsonWithOptions n TestTypes.IndRecStart)
    where
    parseJSON v = do
        Shadow.IndRecStart mf2v i <- genericParseJSON (getOptions @n) v
        mf2 <- case mf2v of
            Nothing -> pure Nothing
            Just rv -> Just . unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.IndRec2) rv
        pure $ ToFromJsonWithOptions $ TestTypes.IndRecStart mf2 i

-- IndRec2: IndRec3

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    ToJSON (ToFromJsonWithOptions n TestTypes.IndRec2)
    where
    toJSON (ToFromJsonWithOptions (TestTypes.IndRec2 f3)) =
        genericToJSON (getOptions @n) $
            Shadow.IndRec2 (toJSON (ToFromJsonWithOptions @n f3))

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    FromJSON (ToFromJsonWithOptions n TestTypes.IndRec2)
    where
    parseJSON v = do
        Shadow.IndRec2 f3v <- genericParseJSON (getOptions @n) v
        f3 <- unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.IndRec3) f3v
        pure $ ToFromJsonWithOptions $ TestTypes.IndRec2 f3

-- IndRec3: IndRecStart

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    ToJSON (ToFromJsonWithOptions n TestTypes.IndRec3)
    where
    toJSON (ToFromJsonWithOptions (TestTypes.IndRec3 start)) =
        genericToJSON (getOptions @n) $
            Shadow.IndRec3 (toJSON (ToFromJsonWithOptions @n start))

instance
    {-# OVERLAPPING #-}
    KnownNat n =>
    FromJSON (ToFromJsonWithOptions n TestTypes.IndRec3)
    where
    parseJSON v = do
        Shadow.IndRec3 sv <- genericParseJSON (getOptions @n) v
        start <- unWrap <$> parseJSON @(ToFromJsonWithOptions n TestTypes.IndRecStart) sv
        pure $ ToFromJsonWithOptions $ TestTypes.IndRec3 start
