{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Yaml qualified as Yaml
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Value (AssetClass, unAssetClass)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import GADA.Contracts.Staking.Locked.OnChain qualified as Locked
import GADA.Contracts.Staking.Locked.Treasury.OnChain qualified as Locked
import GADA.Contracts.Vesting.Api (VestingType (LegacyVesting))
import GADA.Contracts.Vesting.OnChain qualified as Vesting
import GADA.PAB.Deployment (
  ContractsConfig,
  StakingConfig (sLockedOptions, sSixMonthExtensionLockedOptions),
  stakingAuthToken,
  stakingScriptParams,
  typedValidatorHash,
  vestingParams,
 )

getScriptHashAsByteString :: TypedValidator a -> BSL.ByteString
getScriptHashAsByteString = BSL.pack . show . typedValidatorHash

getCurrencySymbolAsByteString :: AssetClass -> BSL.ByteString
getCurrencySymbolAsByteString = BSL.pack . show . fst . unAssetClass

authTokenTests :: String -> ContractsConfig -> TestTree
authTokenTests network config =
  testGroup
    "Auth Token tests"
    [ goldenVsString
        "Lock Staking Auth Token"
        (dir </> "lock-staking-auth-token.golden")
        (pure $ getCurrencySymbolAsByteString (stakingAuthToken sLockedOptions config))
    , goldenVsString
        "Lock Staking Six Months Auth Token"
        (dir </> "lock-staking-six-months-auth-token.golden")
        (pure $ getCurrencySymbolAsByteString (stakingAuthToken sSixMonthExtensionLockedOptions config))
    ]
  where
    dir = "golden-test" </> network

scriptTest :: String -> ContractsConfig -> TestTree
scriptTest network config =
  testGroup
    "Script tests"
    [ goldenVsString
        "Vesting script"
        (dir </> "vesting-script.golden")
        ( maybe (fail "wrong config") (pure . getScriptHashAsByteString . Vesting.vestingTypedScript) $
            vestingParams config LegacyVesting
        )
    , goldenVsString
        "Lock Staking script"
        (dir </> "lock-staking-script.golden")
        (pure $ getScriptHashAsByteString (Locked.lockedStakingScript (stakingScriptParams config)))
    , goldenVsString
        "Lock Staking Treasury script"
        (dir </> "lock-staking-treasury-script.golden")
        (pure $ getScriptHashAsByteString Locked.lockedTreasuryScript)
    , goldenVsString
        "Lock Staking Six Months script"
        (dir </> "lock-staking-six-months-script.golden")
        (pure $ getScriptHashAsByteString (Locked.lockedStakingScript (stakingScriptParams config)))
    , goldenVsString
        "Lock Staking Six Months Treasury script"
        (dir </> "lock-staking-six-months-treasury-script.golden")
        (pure $ getScriptHashAsByteString Locked.lockedTreasuryScript)
    ]
  where
    dir = "golden-test" </> network

main :: IO ()
main = do
  config <- Yaml.decodeFileThrow $ "envs" </> network </> "bootstrap.yaml"
  defaultMain
    ( testGroup
        ("Golden testing for " <> network)
        [scriptTest network config, authTokenTests network config]
    )
  where
#ifdef SHORT_EPOCH
    network = "testnet"
#else
    network = "mainnet"
#endif
