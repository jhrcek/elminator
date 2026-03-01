{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module TestWrapper where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import TestTypes (optionsList)

newtype ToFromJsonWithOptions (optionIndex :: Nat) a
    = ToFromJsonWithOptions {unWrap :: a}

getOptions :: forall n. KnownNat n => Options
getOptions = optionsList !! fromIntegral (natVal (Proxy @n))

instance
    {-# OVERLAPPABLE #-}
    (GToJSON' Value Zero (Rep a), Generic a, KnownNat n) =>
    ToJSON (ToFromJsonWithOptions n a)
    where
    toJSON (ToFromJsonWithOptions a) = genericToJSON (getOptions @n) a

instance
    {-# OVERLAPPABLE #-}
    (GFromJSON Zero (Rep a), Generic a, KnownNat n) =>
    FromJSON (ToFromJsonWithOptions n a)
    where
    parseJSON v = ToFromJsonWithOptions <$> genericParseJSON (getOptions @n) v
