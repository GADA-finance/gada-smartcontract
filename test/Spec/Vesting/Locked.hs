{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Test cases for locked staking contract.
module Spec.Staking.Locked (
  tests,
) where

import Control.Lens ((%~))
import Control.Monad (void)
import Data.Default (def)
import Ledger (
  Address,
  DiffMilliSeconds,
  PubKeyHash,
  ScriptError (EvaluationError),
  ValidationError (ScriptFailure),
  fromMilliSeconds,
 )
import Ledger.Address (unPaymentPubKeyHash)
import Ledger.TimeSlot (scSlotZeroTime)
import Ledger.Value (AssetClass, Value, assetClass, assetClassValue)
import Plutus.Contract (Contract, ContractError)
import Plutus.Contract.Test (
  Wallet,
  assertFailedTransaction,
  assertNoFailedTransactions,
  mockWalletPaymentPubKeyHash,
  w1,
  w2,
  w3,
 )
import Plutus.Trace.Emulator (EmulatorTrace, activateContractWallet, callEndpoint, waitNSlots)
import Test.Tasty (TestTree, testGroup)
import Prelude qualified as H

import GADA.Contracts.Staking.Locked.Api (StakeParams (StakeParams), UnstakeParams (UnstakeParams))
import GADA.Contracts.Staking.Locked.OffChain (
  LockedStakingEndpointModifiers,
  LockedStakingSchema,
  lockedStakingEndpoints,
  mlsStake,
 )
import GADA.Contracts.Staking.Locked.OnChain (authTokenLockedStakingCurrencySymbol, lockedStakingAddress)
import GADA.Contracts.Staking.Locked.Treasury.Api (CreateLockedTreasuryParams (CreateLockedTreasuryParams))
import GADA.Contracts.Staking.Locked.Treasury.OffChain (lockedTreasuryEndpoints)
import GADA.Contracts.Staking.Locked.Treasury.OnChain (lockedTreasuryAddress)
import GADA.Contracts.Staking.Locked.Types (
  LockedOption (LockedOption, loAPY, loJoinDeadline, loLockDuration),
  LockedStakingAuthTokenParams (LockedStakingAuthTokenParams, patAuthTokenName),
  LockedStakingAuthTokenRedeemer,
  LockedStakingParams (LockedStakingParams),
 )
import GADA.Core.Constraints (
  drop,
  each,
  modify,
  select,
  typed,
  untyped,
  _MintValueWithRedeemer,
  _PayToScripts,
  _ValidateIn,
  _mvValue,
  _soOutputs,
 )
import GADA.Core.Token (gadaAsset)
import GADA.Test.PlutusTraceTestingDsl (
  TestConfig (TestConfig, expectedScriptBalance, expectedUserFundChange, initialExtraBalance),
  ada,
  gains,
  has,
  loses,
  testWithConfig,
  (*),
 )

-- APYs can get low, like 0.8%.
-- We can multiply them by an offset to represent in `Integer` for better precision.
-- For example, we can represent 0.8% as 8 with an `pAPYOffset` of 1000.
apyOffset :: H.Integer
apyOffset = 1_000

-- Offset APY numbers for more precision.
-- Turns 0.8% to 8 if the `apyOffset` is 1000.
percent :: H.Float -> H.Integer
percent a = H.round (a H.* H.fromInteger apyOffset H./ 100)

-- | 10%
apy6Months :: H.Integer
apy6Months = percent 10

-- | 15%
apy12Months :: H.Integer
apy12Months = percent 15

-- | Fast-forward 1 month in 1 second.
oneMonthInTestingMs :: DiffMilliSeconds
oneMonthInTestingMs = 1_000

-- | Testing locked options with timeline sped up.
lockedOptions :: [LockedOption]
lockedOptions =
  [ LockedOption
      { loLockDuration = oneMonthInTestingMs H.* 6
      , loAPY = apy6Months
      , loJoinDeadline = scSlotZeroTime def H.+ fromMilliSeconds (oneMonthInTestingMs H.* 8)
      }
  , LockedOption
      { loLockDuration = oneMonthInTestingMs H.* 12
      , loAPY = apy12Months
      , loJoinDeadline = scSlotZeroTime def H.+ fromMilliSeconds oneMonthInTestingMs
      }
  ]

-- | The locked staking auth token params.
lockedStakingAuthTokenParams :: LockedStakingAuthTokenParams
lockedStakingAuthTokenParams =
  LockedStakingAuthTokenParams "locked-staking-auth" (lockedStakingAddress lockedStakingParams) lockedOptions

-- | The auth token of the locked staking position.
lockedStakingTokenAsset :: AssetClass
lockedStakingTokenAsset =
  assetClass
    (authTokenLockedStakingCurrencySymbol lockedStakingAuthTokenParams)
    (patAuthTokenName lockedStakingAuthTokenParams)

-- | The amount of ADA reserved for creating the staking position UTxO
-- and for the bots to send withdrawn funds to the user's wallet.
reservedAda :: Value
reservedAda = 2_000_000 * ada

-- | The locked staking params.
lockedStakingParams :: LockedStakingParams
lockedStakingParams = LockedStakingParams gadaAsset apyOffset lockedTreasuryAddress

-- | The address of the locked staking script
lockedStakingScriptAddress :: Ledger.Address
lockedStakingScriptAddress = lockedStakingAddress lockedStakingParams

-- | The parameterized locked endpoints.
lockedEndpoints :: LockedStakingEndpointModifiers -> Contract () LockedStakingSchema ContractError ()
lockedEndpoints mods = lockedStakingEndpoints mods lockedStakingAuthTokenParams lockedStakingParams

-- | Get public key hash of a wallet
walletPKH :: Wallet -> PubKeyHash
walletPKH = unPaymentPubKeyHash H.. mockWalletPaymentPubKeyHash

-- TODO: Avoid magic numbers by using property checking tests

-- | All the test cases.
tests :: TestTree
tests =
  testGroup
    "Locked staking tests"
    [ testStake
    , testUnstake
    , testStakePassJoinDeadline
    , testStakeWithoutValidityRange
    , testStakeWithTwoMintedAuthTokens
    , testStakeTwoPositions
    ]

-- | Open a locked position correctly.
testStake :: TestTree
testStake =
  let staker = w1
   in testWithConfig
        "Stake correctly"
        TestConfig
          { initialExtraBalance =
              [ staker `has` [10_000_000 * gadaAsset]
              ]
          , expectedUserFundChange =
              [ staker `loses` [1_000_000 * gadaAsset, reservedAda]
              ]
          , expectedScriptBalance =
              [ lockedStakingScriptAddress `has` [1_000_000 * gadaAsset, 1 * lockedStakingTokenAsset, reservedAda]
              ]
          }
        assertNoFailedTransactions
        (stakeTrace def staker (StakeParams 0 1_000_000 (walletPKH staker)))

-- | Close a locked staking position correctly.
testUnstake :: TestTree
testUnstake =
  let (stakerA, stakerB, operator) = (w1, w2, w3)
   in testWithConfig
        "Unstake correctly"
        TestConfig
          { initialExtraBalance =
              [ stakerA `has` [10_000_000 * gadaAsset]
              , stakerB `has` [10_000_000 * gadaAsset]
              , operator `has` [1_000_000 * gadaAsset]
              ]
          , expectedUserFundChange =
              [ stakerA `gains` [100_000 * gadaAsset]
              , stakerB `loses` [2_000_000 * gadaAsset, reservedAda]
              ]
          , expectedScriptBalance =
              [ lockedStakingScriptAddress `has` [2_000_000 * gadaAsset, reservedAda, 1 * lockedStakingTokenAsset]
              , lockedTreasuryAddress `has` [900_000 * gadaAsset, reservedAda, 1 * lockedStakingTokenAsset]
              ]
          }
        assertNoFailedTransactions
        H.$ do
          treasuryPoolTrace operator 1_000_000
          stakeTrace def stakerA (StakeParams 0 1_000_000 (walletPKH stakerA))
          stakeTrace def stakerB (StakeParams 0 2_000_000 (walletPKH stakerB))
          void H.$ waitNSlots 6
          unstakeTrace def stakerA

-- | Join deadline has passed -> Open a locked position failed.
testStakePassJoinDeadline :: TestTree
testStakePassJoinDeadline =
  let staker = w1
   in testWithConfig
        "Failed to stake pass join deadline"
        TestConfig
          { initialExtraBalance =
              [ staker `has` [10_000_000 * gadaAsset]
              ]
          , expectedUserFundChange = []
          , expectedScriptBalance = []
          }
        ( assertFailedTransaction
            ( \_ err _ -> case err of
                ScriptFailure (EvaluationError ["Join deadline has passed", _] _) -> H.True
                _ -> H.False
            )
        )
        H.$ do
          void H.$ waitNSlots 8
          stakeTrace def staker (StakeParams 0 1_000_000 (walletPKH staker))

-- | Open a locked position after term expiration.
testStakeWithoutValidityRange :: TestTree
testStakeWithoutValidityRange =
  let staker = w1
      mods = def {mlsStake = each H.$ select [drop _ValidateIn]} -- Drop @ValidateIn@ instructions
   in testWithConfig
        "Stake without validity range"
        TestConfig
          { initialExtraBalance =
              [ staker `has` [10_000_000 * gadaAsset]
              ]
          , expectedUserFundChange =
              [ staker `gains` []
              ]
          , expectedScriptBalance =
              [ lockedStakingScriptAddress `has` []
              ]
          }
        ( assertFailedTransaction
            ( \_ err _ -> case err of
                ScriptFailure (EvaluationError ["Invalid withdrawal time"] _) -> H.True
                _ -> H.False
            )
        )
        (stakeTrace mods staker (StakeParams 0 1_000_000 (walletPKH staker)))

-- | Open a locked position with 2 minted auth tokens.
testStakeWithTwoMintedAuthTokens :: TestTree
testStakeWithTwoMintedAuthTokens =
  let staker = w1
      mods =
        def
          { mlsStake =
              each H.$
                select
                  [ -- Double minted value in instruction matching redeemer of type @LockedStakingAuthTokenRedeemer@
                    modify (untyped @LockedStakingAuthTokenRedeemer _MintValueWithRedeemer) H.$
                      _mvValue %~ \oldValue -> oldValue H.<> oldValue
                  ]
          }
   in testWithConfig
        "Stake with 2 minted auth tokens"
        TestConfig
          { initialExtraBalance =
              [ staker `has` [10_000_000 * gadaAsset]
              ]
          , expectedUserFundChange =
              [ staker `gains` []
              ]
          , expectedScriptBalance =
              [ lockedStakingScriptAddress `has` []
              ]
          }
        ( assertFailedTransaction
            ( \_ err _ -> case err of
                ScriptFailure (EvaluationError ["Must mint exactly one auth token", _] _) -> H.True
                _ -> H.False
            )
        )
        (stakeTrace mods staker (StakeParams 0 1_000_000 (walletPKH staker)))

-- | Open a locked position with 2 minted auth tokens.
testStakeTwoPositions :: TestTree
testStakeTwoPositions =
  let staker = w1
      mods =
        def
          { mlsStake =
              each H.$
                select
                  [ -- Double minted value in instruction matching redeemer of type @LockedStakingAuthTokenRedeemer@
                    modify (untyped @LockedStakingAuthTokenRedeemer _MintValueWithRedeemer) H.$
                      _mvValue %~ \oldValue -> oldValue H.<> oldValue
                  , -- Replicate output list in @PayToScripts@ instruction
                    modify (typed _PayToScripts) H.$
                      _soOutputs %~ \oldSoOutputs -> oldSoOutputs H.<> oldSoOutputs
                  ]
          }
   in testWithConfig
        "Stake 2 positions"
        TestConfig
          { initialExtraBalance =
              [ staker `has` [10_000_000 * gadaAsset]
              ]
          , expectedUserFundChange =
              [ staker `gains` []
              ]
          , expectedScriptBalance =
              [ lockedStakingScriptAddress `has` []
              ]
          }
        ( assertFailedTransaction
            ( \_ err _ -> case err of
                ScriptFailure (EvaluationError ["Must mint exactly one auth token", _] _) -> H.True
                _ -> H.False
            )
        )
        (stakeTrace mods staker (StakeParams 0 1_000_000 (walletPKH staker)))

-- | The trace that stake some GADA token
stakeTrace :: LockedStakingEndpointModifiers -> Wallet -> StakeParams -> EmulatorTrace ()
stakeTrace mods w params = do
  hdl <- activateContractWallet w (lockedEndpoints mods)
  callEndpoint @"Stake" hdl params
  void H.$ waitNSlots 1

-- | The trace that unstake a stake position
unstakeTrace :: LockedStakingEndpointModifiers -> Wallet -> EmulatorTrace ()
unstakeTrace mods w = do
  let params = UnstakeParams H.$ walletPKH w
  hdl <- activateContractWallet w (lockedEndpoints mods)
  callEndpoint @"Unstake" hdl params
  void H.$ waitNSlots 1

-- | Transfer asset from operator's wallet to treasury.
treasuryPoolTrace :: Wallet -> H.Integer -> EmulatorTrace ()
treasuryPoolTrace w amount = do
  let params = CreateLockedTreasuryParams lockedStakingTokenAsset (assetClassValue gadaAsset amount)
  hdl <- activateContractWallet w lockedTreasuryEndpoints
  callEndpoint @"CreateTreasury" hdl params
  void H.$ waitNSlots 1
