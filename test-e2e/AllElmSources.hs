{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module AllElmSources (allElmSources) where

import Elminator
import Language.Haskell.TH
import TestTypes
#if MIN_VERSION_ghc_prim(0,11,0)
import GHC.Tuple (Unit, Tuple2, Tuple3) -- needed for TH lookupTypeName in GHC 9.12+
#endif

allElmSources :: [String]
allElmSources =
  $(do exps <- mapM (\opts -> generateFor Elm0p19 opts "Autogen" Nothing builder) optionsList
       pure (ListE exps))
