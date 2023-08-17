{-# LANGUAGE OverloadedStrings #-}

-- | Generate Elm type definitions, encoders and decoders from Haskell data types.
module Elminator
    ( module Elminator
    , ElmVersion (..)
    , HType (..)
    , ToHType (..)
    , ExInfo (..)
    , Builder
    , GenOption (..)
    , PolyConfig (..)
    ) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Writer (WriterT (runWriterT))
import Data.Aeson (Options)
import Data.Foldable (for_)
import Data.Proxy (Proxy)
import Data.Text (Text)
import Elminator.Generics.Simple (ExInfo (..), ExItem, HType (..), ModuleName, SymbolName, ToHType (..), UDefData (UDefData))
import Elminator.Lib (Builder, ElmVersion (..), GenM, GenOption (..), PolyConfig (..))
import Language.Haskell.TH (Exp (LitE), Lit (StringL), Q, runIO)

import qualified Control.Monad.State.Lazy as LState
import qualified Control.Monad.State.Strict as SState
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Elminator.ELM.Generator as Elm

{-
GHCI play:

import Text.Show.Pretty
import qualified Control.Monad.State.Strict as SS
import qualified Control.Monad.State.Lazy as SL
import qualified Data.Set as Set
import Data.Proxy
import GHC.Generics
:set -XDeriveAnyClass -XDeriveGeneric -XTemplateHaskell

data Person phanton a = Person {age :: Int, val :: a} deriving (Show, Generic, ToHType)
personProxy = Proxy :: Proxy (Person Float Bool)
ht = SS.evalState (toHType personProxy) Set.empty
builder = include personProxy (Everything Poly)
genConfig = SL.execState builder Map.empty
putStrLn $(stringE . ppShow =<< runReaderT (runWriterT $ toTypeDescriptor ht) (Elm0p19 , genConfig))
-}

-- | Include the elm source for the Haskell type specified by the proxy argument.
include ::
    ToHType a =>
    Proxy a ->
    -- | decides which components (Elm type definition/Json Encoder/Decoder)
    -- will be included and if the generated type will be polymorphic.
    GenOption ->
    Builder
include p go =
    case SState.evalState (toHType p) Set.empty of
        hType@(HUDef (UDefData mdata _ _)) ->
            LState.modify $ Map.insertWith (\(go1, ht) (go2, _) -> (go2 ++ go1, ht)) mdata ([go], hType)
        HPrimitive _ -> error "Direct encoding of primitive type is not supported"
        HMaybe _ -> error "Direct encoding of maybe type is not supported"
        HList _ -> error "Direct encoding of list type is not supported"
        HRecursive _ -> error "Unexpected meta data"
        HExternal _ -> error "Cannot generate code for external types"

{- | Return the generated Elm code in a template haskell splice and optionally
write to a Elm source file at the same time. The second argument is the Options type
from Aeson library. Use `include` calls to build the `Builder` value.
-}
generateFor ::
    -- | The target Elm version
    ElmVersion ->
    -- | The Aeson.Options
    Options ->
    -- | The name of the target module
    Text ->
    -- | Optional filepath to write the generated source to
    Maybe FilePath ->
    -- | Configuration made by calls to `include` function.
    Builder ->
    Q Exp
generateFor ev opt moduleName mfp builder =
    let gc = LState.execState builder Map.empty
        r = do
            srcs <- mapM generateOne $ Map.elems gc
            front <- Elm.elmFront moduleName
            pure (front, Text.concat srcs)
     in do
            ((front, exprtxt), exinfo) <- runReaderT (runWriterT r) (ev, gc)
            let fSrc = Text.concat [front $ toImport exinfo, "\n\n", exprtxt]
            for_ mfp $ \fp -> runIO $ Text.writeFile fp fSrc
            pure $ toExp fSrc
  where
    toImport :: [ExItem] -> Text
    toImport exs =
        let map_ =
                List.foldr (\(m, s) mp -> Map.insertWith (++) m [s] mp) Map.empty exs
         in Text.intercalate "\n" $ Map.foldrWithKey' foldFn [] map_

    foldFn :: ModuleName -> [SymbolName] -> [Text] -> [Text]
    foldFn mod_ smbs in_ =
        Text.concat ["import ", mod_, " exposing (", Text.intercalate ", " smbs, ")"]
            : in_

    toExp :: Text -> Exp
    toExp = LitE . StringL . Text.unpack

    generateOne :: ([GenOption], HType) -> GenM Text
    generateOne (gs, ht) = do
        srcs <- mapM (generateOne_ ht) gs
        pure $ Text.concat srcs
      where
        generateOne_ :: HType -> GenOption -> GenM Text
        generateOne_ h d = Elm.generateElm d h opt
