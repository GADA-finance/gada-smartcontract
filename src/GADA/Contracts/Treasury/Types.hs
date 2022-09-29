{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core locked staking treasury types.
module GADA.Contracts.Staking.Locked.Treasury.Types (
  LockedTreasuryDatum (..),
  LockedTreasury,
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Typed.Scripts (ValidatorTypes (DatumType, RedeemerType))
import Ledger.Value (AssetClass)
import PlutusTx (makeIsDataIndexed)
import Prelude (Eq, Show)

-- | Datum type of a locked staking treasury output.
newtype LockedTreasuryDatum = LockedTreasuryDatum
  { dLockedStakingAuthToken :: AssetClass
  -- ^ The authentic token class of the staking positions that can withdraw rewards from this treasury.
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Data type for a locked staking treasury.
data LockedTreasury

instance ValidatorTypes LockedTreasury where
  type DatumType LockedTreasury = LockedTreasuryDatum
  type RedeemerType LockedTreasury = ()

-- Make IsData for all script data
makeIsDataIndexed ''LockedTreasuryDatum [('LockedTreasuryDatum, 0)]
