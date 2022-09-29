{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Core off-chain locked staking treasury functions.
module GADA.Contracts.Staking.Locked.Treasury.OffChain (
  lockedTreasuryEndpoints,
  withdrawLockedRewards,
) where

import Control.Monad (when)
import Data.List (sortOn)
import Data.Map qualified as M
import Ledger (ChainIndexTxOut, Datum (Datum), TxOutRef, _ciTxOutScriptDatum, _ciTxOutValue)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Value (Value, assetClassValue, assetClassValueOf)
import Plutus.Contract (Contract, ContractError, Endpoint, Promise, endpoint, handleError, throwError, utxosAt)
import Plutus.Script.Utils.Scripts (datumHash)
import PlutusTx (toBuiltinData)
import Prelude qualified as H

import GADA.Contracts.Staking.Locked.Treasury.Api (
  CreateLockedTreasuryParams (CreateLockedTreasuryParams, cltAuthToken, cltValue),
  WithdrawLockedTreasuryParams (WithdrawLockedTreasuryParams, wltAmount, wltAsset, wltAuthToken),
 )
import GADA.Contracts.Staking.Locked.Treasury.OnChain (lockedTreasuryAddress, lockedTreasuryScript)
import GADA.Contracts.Staking.Locked.Treasury.Types (LockedTreasuryDatum (LockedTreasuryDatum))
import GADA.Core.Tx (TxPair, mustPayToOtherScripts, mustPayToScripts, mustSpendScriptOutputs, submitTxPairs)
import GADA.Core.Utils (logThenThrow)

-- | The minimum amount of Lovelace required at a locked treasury output.
--
-- It is 2_000_000 (2 ADA) at the moment.
minADAInTreasuryOutput :: H.Integer
minADAInTreasuryOutput = 2_000_000

-- | Schema of the locked staking treasury endpoints.
type TreasuryPoolSchema = Endpoint "CreateTreasury" CreateLockedTreasuryParams

-- | Handle the locked staking treasury endpoints.
lockedTreasuryEndpoints :: Promise () TreasuryPoolSchema ContractError ()
lockedTreasuryEndpoints = endpoint @"CreateTreasury" H.$ handleError logThenThrow H.. createLockedTreasury

-- | Create a locked staking treasury output.
createLockedTreasury :: CreateLockedTreasuryParams -> Contract () TreasuryPoolSchema ContractError ()
createLockedTreasury CreateLockedTreasuryParams {cltAuthToken, cltValue} =
  submitTxPairs
    [ mustPayToScripts
        lockedTreasuryScript
        [(LockedTreasuryDatum cltAuthToken, cltValue H.<> lovelaceValueOf minADAInTreasuryOutput)]
    ]

-- | Treasury constraints for a transaction consuming a number of treasury outputs
-- that cover the rewards the user wanting to withdraw.
--
-- The user must:
--
-- - Redeposit the remaining rewards to a new output.
--
-- - Transfer the "unlocked" auth token there.
--
-- - Add a minimum amount of ADA to maintain the treasury.
--
-- The user can keep everything else, which should be a few channges in ADA from previous outputs.
withdrawLockedRewards :: forall a w s. WithdrawLockedTreasuryParams -> Contract w s ContractError [TxPair a]
withdrawLockedRewards WithdrawLockedTreasuryParams {wltAsset, wltAmount, wltAuthToken} = do
  utxos <- utxosAt lockedTreasuryAddress
  let (outputs, totalValue) = selectBestOutputs (filterAuthOutputs utxos)
  when (H.null outputs) H.$ throwError "No treasury available"
  when (assetClassValueOf totalValue wltAsset H.< wltAmount) H.$ throwError "Not enough treasury"
  H.pure
    [ mustSpendScriptOutputs lockedTreasuryScript H.$ (\(oref, o) -> (oref, o, ())) H.<$> outputs
    , mustPayToOtherScripts
        [
          ( lockedTreasuryScript
          ,
            [
              ( rawDatum
              , assetClassValue wltAsset (assetClassValueOf totalValue wltAsset H.- wltAmount)
                  H.<> assetClassValue wltAuthToken (assetClassValueOf totalValue wltAuthToken H.+ 1)
                  H.<> lovelaceValueOf minADAInTreasuryOutput
              )
            ]
          )
        ]
    ]
  where
    -- The expected locked treasury datum to find.
    rawDatum = LockedTreasuryDatum wltAuthToken
    datum = Datum H.$ toBuiltinData rawDatum

    -- Filter out authentic treasury outputs.
    filterAuthOutputs :: M.Map k ChainIndexTxOut -> [(k, ChainIndexTxOut)]
    filterAuthOutputs =
      M.toList H.. M.filter (\(_ciTxOutScriptDatum -> (dh, mD)) -> dh H.== datumHash datum H.|| mD H.== H.Just datum)

    -- Given the authentic treasury outputs, select a subset to fulfill the withdrawal if possible.
    -- TODO: Short circuit, and better selection to avoid congestion?
    selectBestOutputs :: [(TxOutRef, ChainIndexTxOut)] -> ([(TxOutRef, ChainIndexTxOut)], Value)
    selectBestOutputs = H.foldr consider ([], H.mempty) H.. sortOn getAmount
      where
        getAmount (_, o) = assetClassValueOf (_ciTxOutValue o) wltAsset
        consider (oref, o) (os, accumVal) =
          let val = _ciTxOutValue o
              isNeeded = assetClassValueOf val wltAsset H.> 0 H.&& assetClassValueOf accumVal wltAsset H.< wltAmount
           in if H.null os H.|| isNeeded
                then ((oref, o) : os, accumVal H.<> val)
                else (os, accumVal)
