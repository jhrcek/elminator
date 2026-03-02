{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AllElmSources (allElmSources) where

import Elminator
import Language.Haskell.TH
import TestTypes

allElmSources :: [String]
allElmSources =
    $( do
        exps <- mapM (\opts -> generateFor Elm0p19 opts "Autogen" Nothing builder) optionsList
        pure (ListE exps)
     )
