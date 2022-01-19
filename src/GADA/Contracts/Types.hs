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
import Data.Aeson (FromJSON, ToJSON)

data SeedSaleParams = SeedSaleParams
  { -- | GADA token
    pGADAAsset :: AssetClass
    -- | The asset class of the authentic token that identifies legit `Seed Sale Position`s.
  , pAuthToken :: AssetClass
    -- | Master key
  , pOperatorPKH :: PubKeyHash
  }
  deriving stock (Generic, Show)

-- | Datum type of a seed sale GADA position.
data SeedSaleDatum = SeedSaleDatum
  { dListSale :: [(PubKeyHash, (Integer, Integer))]
  , dRate :: Integer
  , dAmountPerMonth :: Integer
  , dMaxAmount :: Integer
  , dNumContract :: Integer
  , dStart :: POSIXTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Redeemer type of a seed sale GADA position.
data SeedSaleRedeemer
  = Update
  | Withdraw PubKeyHash Integer
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


-- | Data type for a seed sale GADA position.
data SeedSalePosition

instance ValidatorTypes SeedSalePosition where
  type DatumType SeedSalePosition = SeedSaleDatum
  type RedeemerType SeedSalePosition = SeedSaleRedeemer

-- | Parameters for the seed sale GADA auth token's minting policy.
--
-- Only need the public key hash of the fully trusted operator
-- that can mint and burn this authentic token.
newtype SeedSaleAuthTokenParams = SeedSaleAuthTokenParams {
  tpOperatorPKH :: PubKeyHash
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


-- | Offchain parameters for creating a seed sale GADA position.
--
-- Only need the initial datum.
data CreateSeedSaleParams = CreateSeedSaleParams {
    cpOperatorPKH :: PubKeyHash
  , cpAmountGADA :: Integer
  , cpInitialDatum :: SeedSaleDatum
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

-- | Offchain parameters for updating a seed sale GADA position.
data UpdateSeedSaleParams = UpdateSeedSaleParams
  { upNumContract :: Integer
  , upNewDatum :: SeedSaleDatum
  , upNewAmount :: Maybe Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


-- | Offchain parameters for withdrawing from a seed sale GADA position.
data WithdrawSeedSaleParams = WithdrawSeedSaleParams
  { -- | The withdrawal GADA amount.
    wpWithdrawAmount :: Integer
    -- | An optional submit time. If this is `Nothing`,
    -- the backend will calculate the time for the frontend.
  , wpSubmitTime :: Maybe POSIXTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)



-- Make IsData
unstableMakeIsData ''SeedSaleDatum
unstableMakeIsData ''SeedSaleRedeemer
unstableMakeIsData ''SeedSaleParams
unstableMakeIsData ''SeedSaleAuthTokenParams

-- Make Lift
makeLift ''SeedSaleParams
makeLift ''SeedSaleAuthTokenParams
makeLift ''SeedSaleDatum
makeLift ''SeedSaleRedeemer