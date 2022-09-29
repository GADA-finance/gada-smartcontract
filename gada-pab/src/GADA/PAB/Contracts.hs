{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module GADA.PAB.Contracts (
  Contracts (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)
import Language.PureScript.Bridge (argonaut, equal, genericShow, mkSumType)
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run.PSGenerator (HasPSTypes (psTypes))
import Prettyprinter (Pretty, pretty, viaShow)

import GADA.Contracts.Staking.Locked.OffChain qualified as Locked
import GADA.Contracts.Staking.Locked.Treasury.OffChain qualified as Locked
import GADA.Contracts.Staking.Locked.Types qualified as Locked
import GADA.Contracts.Vesting.OffChain qualified as Vesting
import GADA.Contracts.Vesting.Types qualified as Vesting

data Contracts
  = Vesting Vesting.VestingParams
  | LockedStaking Locked.LockedStakingAuthTokenParams Locked.LockedStakingParams
  | LockedStakingTreasury
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance Pretty Contracts where
  pretty = viaShow

instance HasPSTypes Contracts where
  psTypes = [equal . genericShow . argonaut $ mkSumType @Contracts]

instance Builtin.HasDefinitions Contracts where
  getDefinitions = []
  getSchema = mempty
  getContract = getContracts

getContracts :: Contracts -> Builtin.SomeBuiltin
getContracts (Vesting params) = Builtin.SomeBuiltin $ Vesting.vestingEndpoints params
getContracts (LockedStaking policyParams scriptParams) =
  Builtin.SomeBuiltin $ Locked.lockedStakingEndpoints def policyParams scriptParams
getContracts LockedStakingTreasury = Builtin.SomeBuiltin Locked.lockedTreasuryEndpoints
