port module Main exposing (..)

import Autogen
import Json.Decode as D
import Json.Encode as E
import Platform


port output : E.Value -> Cmd msg


type alias Flags =
    E.Value


type alias Model =
    ()


type Msg
    = NoOp


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        results =
            E.object
                (List.filterMap identity
                    [ roundTrip "SingleCon" Autogen.decodeSingleCon Autogen.encodeSingleCon flags
                    , roundTrip "SingleRecCon" Autogen.decodeSingleRecCon Autogen.encodeSingleRecCon flags
                    , roundTrip "SingleConOneField" Autogen.decodeSingleConOneField Autogen.encodeSingleConOneField flags
                    , roundTrip "SingleRecConOneField" Autogen.decodeSingleRecConOneField Autogen.encodeSingleRecConOneField flags
                    , roundTrip "TwoCons1" Autogen.decodeTwoCons Autogen.encodeTwoCons flags
                    , roundTrip "TwoCons2" Autogen.decodeTwoCons Autogen.encodeTwoCons flags
                    , roundTrip "TwoRecCons1" Autogen.decodeTwoRecCons Autogen.encodeTwoRecCons flags
                    , roundTrip "TwoRecCons2" Autogen.decodeTwoRecCons Autogen.encodeTwoRecCons flags
                    , roundTrip "BigCon" Autogen.decodeBigCon Autogen.encodeBigCon flags
                    , roundTrip "BigRecCon" Autogen.decodeBigRecCon Autogen.encodeBigRecCon flags
                    , roundTrip "MixedCons1" Autogen.decodeMixedCons Autogen.encodeMixedCons flags
                    , roundTrip "MixedCons2" Autogen.decodeMixedCons Autogen.encodeMixedCons flags
                    , roundTrip "Comment" Autogen.decodeComment Autogen.encodeComment flags
                    , roundTrip "WithMaybes" Autogen.decodeWithMaybes Autogen.encodeWithMaybes flags
                    , roundTrip "WithMaybesWithNothing" Autogen.decodeWithMaybes Autogen.encodeWithMaybes flags
                    , roundTrip "WithSimpleMaybesC1" Autogen.decodeWithSimpleMaybes Autogen.encodeWithSimpleMaybes flags
                    , roundTrip "WithSimpleMaybesC2" Autogen.decodeWithSimpleMaybes Autogen.encodeWithSimpleMaybes flags
                    , roundTrip "WithMaybesPoly" Autogen.decodeWithMaybesPoly Autogen.encodeWithMaybesPoly flags
                    , roundTrip "Phantom" Autogen.decodePhantom Autogen.encodePhantom flags
                    , roundTrip "RecWithList" Autogen.decodeRecWithList Autogen.encodeRecWithList flags
                    , roundTrip "IndRecStart" Autogen.decodeIndRecStart Autogen.encodeIndRecStart flags
                    , roundTrip "NTSingleCon" Autogen.decodeNTSingleCon Autogen.encodeNTSingleCon flags
                    , roundTrip "NTSingleCon2" Autogen.decodeNTSingleCon2 Autogen.encodeNTSingleCon2 flags
                    , roundTrip "Tuples" Autogen.decodeTuples Autogen.encodeTuples flags
                    , roundTrip "NestedTuples" Autogen.decodeNestedTuples Autogen.encodeNestedTuples flags
                    , roundTrip "WithEmptyTuple" Autogen.decodeWithEmptyTuple Autogen.encodeWithEmptyTuple flags
                    ]
                )
    in
    ( (), output results )


roundTrip : String -> D.Decoder a -> (a -> E.Value) -> E.Value -> Maybe ( String, E.Value )
roundTrip name decoder encoder flags =
    case D.decodeValue (D.field name D.value) flags of
        Err _ ->
            Nothing

        Ok jsonVal ->
            case D.decodeValue decoder jsonVal of
                Err e ->
                    Just ( name, E.object [ ( "error", E.string (D.errorToString e) ) ] )

                Ok val ->
                    Just ( name, encoder val )
