{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import AllElmSources (allElmSources)
import Control.Monad (forM_, when)
import Data.Aeson (Value, decode, encode, object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.String (fromString)
import GHC.TypeLits (KnownNat, SomeNat (..), someNatVal)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit
import TestInstances ()
import TestTypes (optionsList)
import TestValues (PreparedTestCase (..), mkTestCases)

main :: IO ()
main =
    defaultMain $
        testGroup "E2E Round-trip" $
            zipWith
                ( \i opts -> case someNatVal (fromIntegral i) of
                    Just (SomeNat (p :: Proxy n)) ->
                        testCase
                            ("Options " ++ show i ++ "{" ++ showOpts opts ++ "}")
                            (runTest i p)
                    Nothing -> error $ "someNatVal failed for " ++ show i
                )
                [0 :: Int ..]
                optionsList

showOpts :: A.Options -> String
showOpts o =
    let flm = "fieldLabelModifier: " ++ if A.fieldLabelModifier o "x" == "x" then "default" else "YES"
        ctm = "constructorTagModifier: " ++ if A.constructorTagModifier o "x" == "x" then "default" else "YES"
        nul = "allNullaryToStringTag: " ++ show (A.allNullaryToStringTag o)
        omit = "omitNothingFields: " ++ show (A.omitNothingFields o)
        uwr = "unwrapUnaryRecords: " ++ show (A.unwrapUnaryRecords o)
        tsc = "tagSingleConstructors: " ++ show (A.tagSingleConstructors o)
        se =
            "sumEncoding: " ++ case A.sumEncoding o of
                A.TaggedObject t c -> "TaggedObject(" ++ t ++ ", " ++ c ++ ")"
                A.UntaggedValue -> "UntaggedValue"
                A.ObjectWithSingleField -> "ObjectWithSingleField"
                A.TwoElemArray -> "TwoElemArray"
     in intercalate ", " [flm, ctm, nul, omit, se, uwr, tsc]

runTest :: forall n. KnownNat n => Int -> Proxy n -> IO ()
runTest i _ = do
    let cases = mkTestCases (Proxy @n)
        elmSrc = allElmSources !! i
        inputJson = object [fromString (ptcName c) .= ptcEncoded c | c <- cases]

    -- Write Autogen.elm
    createDirectoryIfMissing True "test-e2e/elm-app/src"
    writeFile "test-e2e/elm-app/src/Autogen.elm" elmSrc

    -- Compile Elm (must run from elm-app dir where elm.json lives)
    (ec, _, stderr_) <-
        readCreateProcessWithExitCode
            (proc "elm" ["make", "--optimize", "--output=elm.js", "src/Main.elm"])
                { cwd = Just "test-e2e/elm-app"
                }
            ""
    when (ec /= ExitSuccess) $ assertFailure $ "elm make failed: " ++ stderr_

    -- Write input JSON
    BL.writeFile "test-e2e/elm-app/input.json" (encode inputJson)

    -- Run Node.js
    (ec2, stdout_, stderr2) <- readProcessWithExitCode "node" ["test-e2e/runner.js"] ""
    when (ec2 /= ExitSuccess) $ assertFailure $ "node runner failed: " ++ stderr2

    -- Parse results
    let results = decode (BL8.pack stdout_) :: Maybe (Map String Value)
    case results of
        Nothing -> assertFailure "Could not parse Elm output as JSON"
        Just rs -> forM_ cases $ \c ->
            case Map.lookup (ptcName c) rs of
                Nothing -> assertFailure $ ptcName c ++ ": missing from Elm output"
                Just v -> case ptcVerify c v of
                    Left err -> assertFailure err
                    Right () -> pure ()
