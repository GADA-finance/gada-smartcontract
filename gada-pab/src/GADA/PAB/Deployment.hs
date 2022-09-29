{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module GADA.PAB.Deployment (
  ContractsConfig (..),
  VestingConfig (..),
  StakingConfig (..),
  vestingParams,
  stakingAuthToken,
  createLockedStakingTreasuryParams,
  stakingTokenParams,
  stakingScriptParams,
  typedValidatorHash,
  stakingAuthTokenMintingPolicy,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map, (!?))
import GHC.Generics (Generic)
import Ledger (PubKeyHash)
import Ledger.Scripts (ValidatorHash, validatorHash)
import Ledger.Typed.Scripts (MintingPolicy, TypedValidator, validatorScript)
import Ledger.Value (AssetClass, TokenName, assetClass, assetClassValue)

import GADA.Contracts.Staking.Locked.OnChain qualified as Locked
import GADA.Contracts.Staking.Locked.Treasury.Api qualified as Locked
import GADA.Contracts.Staking.Locked.Treasury.OnChain qualified as Locked
import GADA.Contracts.Staking.Locked.Types qualified as Locked
import GADA.Contracts.Vesting.Api (VestingType)
import GADA.Contracts.Vesting.Types qualified as Vesting

data ContractsConfig = ContractsConfig
  { gadaAsset :: AssetClass
  , vesting :: VestingConfig
  , staking :: StakingConfig
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype VestingConfig = VestingConfig (Map VestingType PubKeyHash)
  deriving stock (Generic, Show)
  deriving newtype (FromJSON, ToJSON)

data StakingConfig = StakingConfig
  { sAPYOffset :: Integer
  , sLockedOptions :: [Locked.LockedOption]
  , sSixMonthExtensionLockedOptions :: [Locked.LockedOption]
  , sSixMonthExtensionLockedOptions01012023 :: [Locked.LockedOption]
  , sAuthTokenName :: TokenName
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

vestingParams :: ContractsConfig -> VestingType -> Maybe Vesting.VestingParams
vestingParams ContractsConfig {gadaAsset, vesting = VestingConfig operators} vestingType =
  Vesting.VestingParams <$> (operators !? vestingType) <*> pure gadaAsset

stakingAuthToken :: (StakingConfig -> [Locked.LockedOption]) -> ContractsConfig -> AssetClass
stakingAuthToken getOptions config@ContractsConfig {staking = StakingConfig {sAuthTokenName}} =
  assetClass
    (Locked.authTokenLockedStakingCurrencySymbol . stakingTokenParams getOptions $ config)
    sAuthTokenName

stakingAuthTokenMintingPolicy :: (StakingConfig -> [Locked.LockedOption]) -> ContractsConfig -> MintingPolicy
stakingAuthTokenMintingPolicy getOptions = Locked.authTokenLockedStakingPolicy . stakingTokenParams getOptions

stakingScriptParams :: ContractsConfig -> Locked.LockedStakingParams
stakingScriptParams ContractsConfig {gadaAsset, staking = StakingConfig {..}} =
  Locked.LockedStakingParams
    { Locked.pStakeAsset = gadaAsset
    , Locked.pAPYOffset = sAPYOffset
    , Locked.pTreasuryScript = Locked.lockedTreasuryAddress
    }

stakingTokenParams ::
  (StakingConfig -> [Locked.LockedOption]) ->
  ContractsConfig ->
  Locked.LockedStakingAuthTokenParams
stakingTokenParams getOptions config@ContractsConfig {staking = staking@StakingConfig {sAuthTokenName}} =
  Locked.LockedStakingAuthTokenParams
    { Locked.patAuthTokenName = sAuthTokenName
    , Locked.patLockedStakingScriptAddr = Locked.lockedStakingAddress (stakingScriptParams config)
    , Locked.patLockedOptions = getOptions staking
    }

createLockedStakingTreasuryParams ::
  (StakingConfig -> [Locked.LockedOption]) ->
  ContractsConfig ->
  Integer ->
  Locked.CreateLockedTreasuryParams
createLockedStakingTreasuryParams getOptions config@ContractsConfig {gadaAsset} amount =
  Locked.CreateLockedTreasuryParams
    { Locked.cltAuthToken = stakingAuthToken getOptions config
    , Locked.cltValue = assetClassValue gadaAsset amount
    }

typedValidatorHash :: TypedValidator a -> ValidatorHash
typedValidatorHash = validatorHash . validatorScript
