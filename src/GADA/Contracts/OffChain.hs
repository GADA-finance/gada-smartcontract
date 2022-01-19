{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Core off-chain seed sale types and functions.
module GADA.Contracts.OffChain where

import Control.Monad
import Data.Aeson (FromJSON)
import Ledger ( ChainIndexTxOut, TxOutRef, PubKeyHash, PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints qualified as Constraints
import Ledger.Value (assetClassValue, assetClassValueOf)
import Plutus.Contract
import Prelude
import PlutusTx qualified
import  Data.Map
import Data.Maybe

import GADA.Contracts.Common 
import GADA.Contracts.OnChain
import GADA.Contracts.Types 
import GADA.Contracts.Utils 

-- | `findSeedSalePosition` @params pkh@ finds an authentic seed sale position with
-- the passed in `pkh` in white list.
findSeedSalePosition ::
  SeedSaleParams ->
  Integer ->
  Contract w s ContractError (TxOutRef, ChainIndexTxOut, SeedSaleDatum)
findSeedSalePosition params@SeedSaleParams {pAuthToken} numContract =
  maybe (throwError "Seed sale position not found") pure
    =<< findFirstOutput
      (seedSaleAddress params)
      ( \(_, o) -> case getDatum o of
          Just d -> dNumContract d == numContract && assetClassValueOf (getValue o) pAuthToken == 1
          _ -> False
      )

-- | Schema of the seedSale endpoints with create, update, and withdraw.
type SeedSaleSchema =
  Endpoint "CreateSeedSale" CreateSeedSaleParams
    .\/ Endpoint "UpdateSeedSale" UpdateSeedSaleParams
    .\/ Endpoint "WithdrawSeedSale" WithdrawSeedSaleParams

-- | `seedSaleEndpoints` @params@ handles the seedSale endpoints with
-- `createSeedSale`, `updateSeedSale`, and `withdrawSeedSale`.
seedSaleEndpoints :: SeedSaleParams -> Promise () SeedSaleSchema ContractError ()
seedSaleEndpoints seedSaleParams =
  ( handle @"CreateSeedSale" createSeedSale
      `select` handle @"UpdateSeedSale" updateSeedSale
      `select` handle @"WithdrawSeedSale" withdrawSeedSale
  )
    <> seedSaleEndpoints seedSaleParams
  where
    handle ::
      forall l p.
      (HasEndpoint l p SeedSaleSchema, FromJSON p) =>
      (SeedSaleParams -> p -> Contract () SeedSaleSchema ContractError ()) ->
      Promise () SeedSaleSchema ContractError ()
    handle = endpoint @l . (handleError logError .) . ($ seedSaleParams)

-- | `createSeedSale` @seedSaleParams createParams@ submits a transaction that creates a seedSale position.
createSeedSale :: SeedSaleParams -> CreateSeedSaleParams -> Contract w s ContractError ()
createSeedSale seedSaleParams@SeedSaleParams{pAuthToken, pGADAAsset} createSqeedSaleParams@CreateSeedSaleParams{cpOperatorPKH, cpInitialDatum, cpAmountGADA} =
    submitTxPairs
      [ mustMintValue -- Mint an auth token
          [seedSaleAuthTokenPolicy (SeedSaleAuthTokenParams cpOperatorPKH)]
          (assetClassValue pAuthToken 1)
      , mustPayToScripts -- Create a seedSale output with the initial datum and vested amount
          (seedSaleScript seedSaleParams)
          [(cpInitialDatum, minADAOutput <> assetClassValue pAuthToken 1 <> assetClassValue pGADAAsset cpAmountGADA )]
      ]

-- | `updateSeedSale` @seedSaleParams updateParams@ submits a transaction that updates an existing seedSale position.
updateSeedSale :: SeedSaleParams -> UpdateSeedSaleParams -> Contract w s ContractError ()
updateSeedSale
  seedSaleParams@SeedSaleParams{pGADAAsset, pAuthToken}
  updateSeedSaleParams@UpdateSeedSaleParams{upNumContract, upNewDatum, upNewAmount} =
    do
      (oref, o, _) <- findSeedSalePosition seedSaleParams upNumContract
      let inst = seedSaleScript seedSaleParams
          curValue = fromMaybe (assetClassValueOf (getValue o) pGADAAsset) upNewAmount
      submitTxPairs
        [ -- Spend the previous output
          mustSpendScriptOutputs inst [(oref, o, Update)]
        , -- Create a new seedSale output with the new datum and vested amount
          mustPayToScripts inst [(upNewDatum, minADAOutput <> assetClassValue pAuthToken 1 <> assetClassValue pGADAAsset curValue)]
        ]

-- | `withdrawSeedSale` @seedSaleParams withdrawParams@ submits a transaction that withdraws from an existing seedSale position.
withdrawSeedSale :: SeedSaleParams -> WithdrawSeedSaleParams -> Contract w s ContractError ()
withdrawSeedSale seedSaleParams _ = do
  unless (wpWithdrawAmount > 0) (throwError "Must withdraw a positive amount")

  -- Find the positon to withdraw from
  PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
  (oref, o, prevDatum@SeedSaleDatum {dListSale}) <- findSeedSalePosition seedSaleParams pkh

  -- Get submit time and validate withdrawl amount
  submitTime <- case wpSubmitTime of
    Just t -> pure t
    _ -> currentTime
  let validAmount = dWithdrawnAmount + wpWithdrawAmount <= unlockedAmount prevDatum submitTime
  unless validAmount (throwError "Invalid withdrawal amount")

  -- Prepare data then submit the transaction
  let newDatum = prevDatum {dWithdrawnAmount = dWithdrawnAmount + wpWithdrawAmount}
      newValue = seedSaleValue seedSaleParams (dTotalEpochs * dAmountPerEpoch - dWithdrawnAmount - wpWithdrawAmount)
      inst = seedSaleScript seedSaleParams
  submitTxPairs
    [ -- Spend the previous output
      mustSpendScriptOutputs inst [(oref, o, Withdraw wpWithdrawAmount)]
    , -- Pay to the seedSale person
      (mempty, Constraints.mustPayToPubKey (PaymentPubKeyHash pkh) (assetClassValue (pSeedSaleAsset seedSaleParams) wpWithdrawAmount))
    , -- Create a new seedSale output that reflects the withdrawal
      mustPayToScripts inst [(newDatum, newValue)]
    , -- Add a validity interval to have the lower works as submit time
      mustValidateIn (interval submitTime (submitTime + 888_888))
    ]
