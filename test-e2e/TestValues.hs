{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module TestValues where

import Data.Aeson
import Data.Proxy
import GHC.TypeLits
import TestInstances ()
import TestTypes
import TestWrapper

data PreparedTestCase = PreparedTestCase
    { ptcName :: String
    , ptcEncoded :: Value
    , ptcVerify :: Value -> Either String ()
    }

mkTestCases :: forall n. KnownNat n => Proxy n -> [PreparedTestCase]
mkTestCases _ =
    [ mk @SingleCon "SingleCon" (SingleCon (Just 10) "Sras")
    , mk @SingleRecCon "SingleRecCon" (SingleRecCon 10 "Sras")
    , mk @SingleConOneField "SingleConOneField" (SingleConOneField 10)
    , mk @SingleRecConOneField "SingleRecConOneField" (SingleRecConOneField 10)
    , mk @TwoCons "TwoCons1" (TCCon1 10)
    , mk @TwoCons "TwoCons2" (TCCon2 "Sras")
    , mk @TwoRecCons "TwoRecCons1" (RTCCon1 10)
    , mk @TwoRecCons "TwoRecCons2" (RTCCon2 "Sras")
    , mk @BigCon "BigCon" (BigCon 12 23 "asdas" "asdasd" 2.3 12 25 54 21.2 "werwr" "asdaSD" 2.3 12 (TCCon2 "ssad") (SingleRecCon 10 "asdasd"))
    , mk @BigRecCon "BigRecCon" (BigRecCon 12 23 "asdas" "asdasd" 2.3 12 25 54 21.2 "werwr" "asdaSD" 2.3 12 (TCCon2 "ssad") (SingleRecCon 10 "asdasd"))
    , mk @MixedCons "MixedCons1" (MxCon1 10)
    , mk @MixedCons "MixedCons2" (MxCon2 "asdad")
    , mk @Comment "Comment" (Comment "somec" (Just (Comment "somec" Nothing)))
    , mk @WithMaybes "WithMaybes" (WithMaybes (Just 10) (Just "Sras"))
    , mk @WithMaybes "WithMaybesWithNothing" (WithMaybes (Just 10) Nothing)
    , mk @WithSimpleMaybes "WithSimpleMaybesC1" (WithSimpleMaybesC1 (Just 10) Nothing)
    , mk @WithSimpleMaybes "WithSimpleMaybesC2" (WithSimpleMaybesC2 Nothing (Just 10))
    , mk @(WithMaybesPoly (Maybe String) Float) "WithMaybesPoly" (WithMaybesPoly (Just (Just "sras")) (Just 10))
    , mk @(Phantom ()) "Phantom" (Phantom "Sras")
    , mk @RecWithList "RecWithList" (RecWithList ["Sras"] 2.3)
    , mk @IndRecStart "IndRecStart" (IndRecStart (Just (IndRec2 (IndRec3 (IndRecStart Nothing 20)))) 102)
    , mk @NTSingleCon "NTSingleCon" (NTSingleCon 102)
    , mk @NTSingleCon2 "NTSingleCon2" (NTSingleCon2 223)
    , mk @Tuples "Tuples" (Tuples (1, "asd", 4.5))
    , mk @NestedTuples "NestedTuples" (NestedTuples (1, ("asd", 4.5)))
    , mk @(WithEmptyTuple ()) "WithEmptyTuple" (WithEmptyTuple ())
    ]
  where
    mk ::
        forall a.
        ( Eq a
        , FromJSON (ToFromJsonWithOptions n a)
        , Show a
        , ToJSON (ToFromJsonWithOptions n a)
        ) =>
        String -> a -> PreparedTestCase
    mk name val =
        PreparedTestCase
            { ptcName = name
            , ptcEncoded = toJSON (ToFromJsonWithOptions @n val)
            , ptcVerify = \elmJson ->
                case fromJSON @(ToFromJsonWithOptions n a) elmJson of
                    Error e -> Left $ name ++ ": fromJSON failed: " ++ e
                    Success (ToFromJsonWithOptions v) ->
                        if v == val
                            then Right ()
                            else
                                Left $
                                    name
                                        ++ ": mismatch\n  expected: "
                                        ++ show val
                                        ++ "\n  got:      "
                                        ++ show v
            }
