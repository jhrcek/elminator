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
import Control.Monad.State.Lazy
import Control.Monad.Writer
import Data.Aeson (Options)
import Data.Proxy (Proxy)
import Data.Text as T
import Data.Text.IO as T
import Elminator.Generics.Simple
import Elminator.Lib
import Language.Haskell.TH

import qualified Control.Monad.State.Strict as SState
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Elminator.ELM.Generator as Elm

{- | Include the elm source for the Haskell type specified by the proxy argument.
The second argument decides which components will be included and if the
generated type will be polymorphic.
-}
include :: ToHType a => Proxy a -> GenOption -> Builder
include p dc = do
    let hType = SState.evalState (toHType p) Map.empty
    mdata <-
        case hType of
            HUDef (UDefData m _ _) -> pure m
            HPrimitive _ -> error "Direct encoding of primitive type is not supported"
            HMaybe _ -> error "Direct encoding of maybe type is not supported"
            HList _ -> error "Direct encoding of list type is not supported"
            HRecursive _ -> error "Unexpected meta data"
            HExternal _ -> error "Cannot generate code for external types"
    s <- get
    put $ Map.insertWith (\(a, b) (ea, _) -> (ea ++ a, b)) mdata ([dc], hType) s

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
generateFor ev opt moduleName mfp sc =
    let (_, gc) = runState sc Map.empty
        r = do
            srcs <- mapM generateOne $ Map.elems gc
            front <- Elm.elmFront moduleName
            pure (front, T.intercalate "" srcs)
     in do
            ((front, exprtxt), exinfo) <- runReaderT (runWriterT r) (ev, gc)
            let fSrc = T.concat [front $ toImport exinfo, "\n\n", exprtxt]
            case mfp of
                Just fp -> runIO $ T.writeFile fp fSrc
                Nothing -> pure ()
            pure $ toExp fSrc
  where
    toImport :: [ExItem] -> Text
    toImport exs =
        let map_ =
                List.foldr (\(m, s) mp -> Map.insertWith (++) m [s] mp) Map.empty exs
         in T.intercalate "\n" $ Map.foldrWithKey' foldFn [] map_
    foldFn :: Text -> [Text] -> [Text] -> [Text]
    foldFn mod_ smbs in_ =
        T.concat ["import ", mod_, " exposing (", T.intercalate ", " smbs, ")"]
            : in_
    toExp :: Text -> Exp
    toExp t = LitE $ StringL $ unpack t
    generateOne :: ([GenOption], HType) -> GenM Text
    generateOne (gs, ht) = do
        srcs <- mapM (generateOne_ ht) gs
        pure $ T.intercalate "" srcs
      where
        generateOne_ :: HType -> GenOption -> GenM Text
        generateOne_ h d = Elm.generateElm d h opt
