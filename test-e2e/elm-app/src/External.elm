module External exposing (..)

import Json.Decode as D
import Json.Encode as E


type MyExtType a b
    = MyExtType { extTypeField1 : a, extTypeField2 : b }


encodeMaybe : (a -> E.Value) -> Maybe a -> E.Value
encodeMaybe fn ma =
    case ma of
        Just a ->
            fn a

        Nothing ->
            E.null


encodeMyExtType : MyExtType (Maybe ()) Int -> E.Value
encodeMyExtType a =
    case a of
        MyExtType x ->
            E.object
                [ ( "extTypeField1"
                  , encodeMaybe
                        (let
                            encodeTuple0 a01 =
                                let
                                    () =
                                        a01
                                in
                                E.list identity []
                         in
                         encodeTuple0
                        )
                        x.extTypeField1
                  )
                , ( "extTypeField2", E.int x.extTypeField2 )
                ]


decodeMyExtType : D.Decoder (MyExtType (Maybe ()) Int)
decodeMyExtType =
    D.oneOf
        [ let
            mkMyExtType a1 a2 =
                MyExtType { extTypeField1 = a1, extTypeField2 = a2 }
          in
          D.map2 mkMyExtType
            (D.maybe (D.field "extTypeField1" (D.succeed ())))
            (D.field "extTypeField2" D.int)
        ]
