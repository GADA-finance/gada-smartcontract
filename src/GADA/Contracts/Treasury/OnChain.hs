{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Core on-chain locked staking treasury functions.
module GADA.Contracts.Staking.Locked.Treasury.OnChain (
  lockedTreasuryScript,
  lockedTreasuryAddress,
) where

import Ledger (
  Address,
  Datum (Datum),
  ScriptContext (ScriptContext),
  TxInInfo (TxInInfo),
  TxInfo (TxInfo, txInfoInputs),
  TxOut (TxOut, txOutDatumHash, txOutValue),
  findDatumHash,
  scriptContextTxInfo,
  scriptValidatorHashAddress,
  txInInfoResolved,
 )
import Ledger.Typed.Scripts (TypedValidator, mkTypedValidator, mkUntypedValidator, validatorHash)
import Ledger.Value (assetClassValueOf)
import PlutusTx (compile, toBuiltinData)
import PlutusTx.Prelude qualified as P

import GADA.Contracts.Staking.Locked.Treasury.Types (
  LockedTreasury,
  LockedTreasuryDatum (LockedTreasuryDatum, dLockedStakingAuthToken),
 )

-- | `validateLockedTreasury` @datum redeemer ctx@
-- validates the transaction that spends the locked staking treasury position output.
--
-- Per the locked staking specifications, there are only two places a `dLockedStakingAuthToken` can be:
--
-- 1. In a valid `LockedStakingPosition`.
--
-- 2. In other `LockedTreasury`s with the same `dLockedStakingAuthToken` or in general the same datum hash.
--
-- A locked treasury output must be consumed with an authentic `LockedStakingPosition`.
-- The staking position's validator checks the withdrawal amount and transfer of `dLockedStakingAuthToken` to treasury.
--
-- However, we cannot parameterize its address here as it also needs to know the locked treasury's address (cyclic dep).
-- We then simplify the check here to an authentic token in an input with a different datum hash to avoid (2).
{-# INLINEABLE validateLockedTreasury #-}
validateLockedTreasury :: LockedTreasuryDatum -> () -> ScriptContext -> P.Bool
validateLockedTreasury
  datum@LockedTreasuryDatum {dLockedStakingAuthToken}
  _
  ScriptContext {scriptContextTxInfo = info@TxInfo {txInfoInputs}} =
    P.traceIfFalse
      "Auth token in wanted inputs not found"
      ( P.any
          ( \TxInInfo {txInInfoResolved = TxOut {txOutDatumHash, txOutValue}} ->
              txOutDatumHash P./= ownDatumHash P.&& assetClassValueOf txOutValue dLockedStakingAuthToken P.> 0
          )
          txInfoInputs
      )
    where
      ownDatumHash = findDatumHash (Datum (toBuiltinData datum)) info

-- | The compiled locked staking treasury script.
lockedTreasuryScript :: TypedValidator LockedTreasury
lockedTreasuryScript =
  mkTypedValidator @LockedTreasury
    $$(compile [||validateLockedTreasury||])
    $$(compile [||mkUntypedValidator||])

-- | The address of the locked staking treasury script.
lockedTreasuryAddress :: Address
lockedTreasuryAddress = (`scriptValidatorHashAddress` P.Nothing) P.. validatorHash P.$ lockedTreasuryScript
