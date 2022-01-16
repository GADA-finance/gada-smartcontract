{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core Seed Sale types.
module GADA.Contracts.Types where

import GHC.Generics (Generic)
import Ledger (AssetClass, POSIXTime, PubKeyHash)
import Ledger.Typed.Scripts (ValidatorTypes (DatumType, RedeemerType))
import PlutusTx (makeLift, unstableMakeIsData)
import Prelude (Eq, Integer, Maybe, Show)

data SeedSaleParams = SeedSaleParams
  { pSaleAsset :: AssetClass
    -- ^ GADA token
  , pAuthToken :: AssetClass
    -- ^ The asset class of the authentic token that identifies legit `Seed Sale Position`s.
  }
  deriving stock (Generic, Show)

-- | Datum type of a seed sale GADA position.
data SeedSaleDatum = SeedSaleDatum
  { dSalePKH :: [(PubKeyHash,Integer)]
  , dRate :: Integer
  , dAmountPerMonth :: Integer
  , dMaxAmount :: Integer
  }
  deriving stock (Generic, Show)

-- | Redeemer type of a seed sale GADA position.
data SeedSaleRedeemer
  = Update
  | Withdraw Integer
  deriving stock (Generic, Show)

-- | Data type for a seed sale GADA position.
data SeedSalePosition

instance ValidatorTypes SeedSalePosition where
  type DatumType SeedSalePosition = SeedSaleDatum
  type RedeemerType SeedSalePosition = SeedSaleRedeemer

-- | Parameters for the seed sale GADA auth token's minting policy.
--
-- Only need the public key hash of the fully trusted operator
-- that can mint and burn this authentic token.
newtype SeedSaleAuthTokenParams
  = SeedSaleAuthTokenParams { mpOperatorPKH :: PubKeyHash }
  deriving stock (Eq, Generic, Show)

-- Make IsData for all script data
unstableMakeIsData ''SeedSaleParams
unstableMakeIsData ''SeedSaleDatum
unstableMakeIsData ''SeedSaleRedeemer
unstableMakeIsData ''SeedSaleAuthTokenParams

-- Make Lift for script parameters
makeLift ''SeedSaleParams
makeLift ''SeedSaleAuthTokenParams

-- | Offchain parameters for creating a seed sale GADA position.
--
-- Only need the initial datum.
newtype CreateSeedSaleParams
  = CreateSeedSaleParams { cpInitialDatum :: SeedSaleDatum }
  deriving stock (Generic)

-- | Offchain parameters for updating a seed sale GADA position.
data UpdateSeedSaleParams = UpdateSeedSaleParams
  { upCurrentPKH :: PubKeyHash
  , upNewDatum :: SeedSaleDatum
  }
  deriving stock (Generic)

-- | Offchain parameters for withdrawing from a seed sale GADA position.
data WithdrawSeedSaleParams = WithdrawSeedSaleParams
  { wpWithdrawAmount :: Integer
    -- ^ The withdrawal GADA amount.
  , wpSubmitTime :: Maybe POSIXTime
    -- ^ An optional submit time. If this is `Nothing`,
    -- the backend will calculate the time for the frontend.
  }
  deriving stock (Generic)
