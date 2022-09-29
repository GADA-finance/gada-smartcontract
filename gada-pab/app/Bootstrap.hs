{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Api qualified as CA
import Cardano.Api.Shelley qualified as CA
import Codec.Serialise qualified as Codec
import Control.Monad (forM)
import Control.Monad.Extra (eitherM)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Coerce (Coercible, coerce)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple.Extra (uncurry3)
import Data.Yaml qualified as Yaml
import Ledger qualified as Plutus
import Ledger.Tx.CardanoAPI qualified as Ledger
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Scripts (Script)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))

import GADA.Contracts.Staking.Locked.OnChain (lockedStakingAddress)
import GADA.Contracts.Staking.Locked.OnChain qualified as Locked
import GADA.Contracts.Staking.Locked.Treasury.OnChain (lockedTreasuryAddress)
import GADA.Contracts.Staking.Locked.Treasury.OnChain qualified as Locked
import GADA.Contracts.Vesting.Api (VestingType (PrivateSaleVesting, TeamVesting), vestingTypeToScriptName)
import GADA.Contracts.Vesting.OnChain (vestingAddress)
import GADA.Contracts.Vesting.OnChain qualified as Vesting
import GADA.Core.Types (
  PlutusScriptName (
    PsLockedStaking,
    PsLockedStakingAuthToken,
    PsLockedStakingSixMonthAuthToken,
    PsLockedStakingSixMonthAuthToken01012023,
    PsTreasury
  ),
  scriptNameToString,
 )
import GADA.PAB.Deployment (
  StakingConfig (sLockedOptions, sSixMonthExtensionLockedOptions, sSixMonthExtensionLockedOptions01012023),
  stakingAuthTokenMintingPolicy,
  stakingScriptParams,
  vestingParams,
 )

plutusScriptsDir :: String
plutusScriptsDir = "./"

saveScript ::
  PlutusScriptName ->
  CA.Script CA.PlutusScriptV1 ->
  Maybe (CA.AddressInEra CA.BabbageEra) ->
  IO (Text, Map Text Text)
saveScript scriptName script mAddress = do
  let name = scriptNameToString scriptName
  eitherM CA.throwErrorAsException pure $
    CA.writeFileTextEnvelope
      (plutusScriptsDir </> (name <> ".plutus"))
      Nothing
      script
  let mAddressEntry = ("address" :: Text,) . CA.serialiseAddress <$> mAddress
      hashHex = CA.serialiseToRawBytesHexText $ CA.hashScript script
      mapEntries = [("hash", hashHex)] <> maybeToList mAddressEntry
  pure (T.pack name, Map.fromList mapEntries)

typedValidatorToScript :: Scripts.TypedValidator a -> Plutus.Script
typedValidatorToScript = Plutus.unValidatorScript . Scripts.validatorScript

fromPlutusScript ::
  (Coercible script Script) =>
  script ->
  CA.Script CA.PlutusScriptV1
fromPlutusScript =
  CA.PlutusScript
    CA.PlutusScriptV1
    . CA.PlutusScriptSerialised
    . SBS.toShort
    . LBS.toStrict
    . Codec.serialise
    . coerce @_ @Script

main :: IO ()
main = do
  args <- getArgs
  configFile <- case args of
    [p] -> pure p
    _ -> fail "Expect exactly 1 argument - configuration file"
  config <- Yaml.decodeFileThrow configFile
  vestingScripts <- forM [TeamVesting, PrivateSaleVesting] $ \vt ->
    case vestingParams config vt of
      Just params ->
        pure
          ( vestingTypeToScriptName vt
          , fromPlutusScript . Vesting.vestingScript $ params
          , Just $ toCardanoAddressHandleError cardanoNetworkId . vestingAddress $ params
          )
      Nothing -> fail $ "Vesting config for " <> show vt <> " missing!"
  let lockedStakingScript =
        fromPlutusScript $ typedValidatorToScript (Locked.lockedStakingScript (stakingScriptParams config))
      lockedStakingAuthTokenScript =
        fromPlutusScript $ Plutus.unMintingPolicyScript $ stakingAuthTokenMintingPolicy sLockedOptions config
      lockedStakingSixMonthAuthTokenScript =
        fromPlutusScript $
          Plutus.unMintingPolicyScript $ stakingAuthTokenMintingPolicy sSixMonthExtensionLockedOptions config
      lockedStakingSixMonthAuthTokenScript01012023 =
        fromPlutusScript $
          Plutus.unMintingPolicyScript $ stakingAuthTokenMintingPolicy sSixMonthExtensionLockedOptions01012023 config
      lockedStakingTreasuryScript =
        fromPlutusScript $ typedValidatorToScript Locked.lockedTreasuryScript
      lockedStakingAddr =
        toCardanoAddressHandleError cardanoNetworkId $ lockedStakingAddress (stakingScriptParams config)
      lockedStakingTreasuryAddr = toCardanoAddressHandleError cardanoNetworkId lockedTreasuryAddress

      stakingScripts :: [(PlutusScriptName, CA.Script CA.PlutusScriptV1, Maybe (CA.AddressInEra CA.BabbageEra))]
      stakingScripts =
        [ (PsLockedStaking, lockedStakingScript, Just lockedStakingAddr)
        , (PsLockedStakingAuthToken, lockedStakingAuthTokenScript, Nothing)
        , (PsLockedStakingSixMonthAuthToken, lockedStakingSixMonthAuthTokenScript, Nothing)
        , (PsLockedStakingSixMonthAuthToken01012023, lockedStakingSixMonthAuthTokenScript01012023, Nothing)
        , (PsTreasury, lockedStakingTreasuryScript, Just lockedStakingTreasuryAddr)
        ]
  summary <- Map.fromList <$> traverse (uncurry3 saveScript) (vestingScripts <> stakingScripts)
  Yaml.encodeFile (plutusScriptsDir </> "summary.yaml") summary

toCardanoAddressHandleError :: CA.NetworkId -> Plutus.Address -> CA.AddressInEra CA.BabbageEra
toCardanoAddressHandleError networkId addr = either (error . show) id $ Ledger.toCardanoAddressInEra networkId addr

-- TODO: Just support preprod testnet
cardanoNetworkId :: CA.NetworkId
cardanoNetworkId = CA.Testnet (CA.NetworkMagic 1)

-- cardanoNetworkId = CA.Mainnet
