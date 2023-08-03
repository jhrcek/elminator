{-# LANGUAGE OverloadedStrings #-}

module Elminator.ELM.Elm19 where

import Data.Text as T
import Elminator.ELM.Render

elmFront :: Text -> Text -> Text
elmFront moduleName imports =
    T.concat
        [ "\
          \module "
        , moduleName
        , " exposing (..)\n\
          \\n"
        , imports
        , "\n\n"
        , "import Json.Encode as E\n\
          \import Json.Decode as D\n\
          \\n\
          \seqApp : D.Decoder (a1 -> v) -> D.Decoder a1 -> D.Decoder v\n\
          \seqApp inDec oDec =\n\
          \  let\n\
          \    mapFn v = D.map (\\x -> x v) inDec\n\
          \  in D.andThen mapFn oDec\n\
          \\n\
          \elminatorEncodeList0p19 : (a -> E.Value)-> List a -> E.Value\n\
          \elminatorEncodeList0p19 fn ls = E.list fn ls\n\
          \\n\
          \encodeMaybe : (a -> E.Value)-> Maybe a -> E.Value\n\
          \encodeMaybe fn ma = case ma of\n\
          \  Just a -> fn a\n\
          \  Nothing -> E.null"
        ]

listEncoder :: EExpr
listEncoder = "elminatorEncodeList0p19"
