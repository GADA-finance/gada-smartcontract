{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Core on-chain seedSale functions.
module GADA.Contracts.OnChain where

import GADA.Contracts.Common
import GADA.Contracts.Types

import Ledger (
  Address,
  Extended (Finite),
  LowerBound (LowerBound),
  POSIXTime,
  PubKeyHash,
  ScriptContext (ScriptContext),
  TxInfo (TxInfo),
  ivFrom,
  mintingPolicyHash,
  mkMintingPolicyScript,
  scriptValidatorHashAddress,
  scriptContextTxInfo,
  txInfoValidRange,
  txSignedBy,
 )
import Ledger.Typed.Scripts (
  MintingPolicy,
  TypedValidator,
  mkTypedValidatorParam,
  mkUntypedValidator,
  mkUntypedMintingPolicy,
  validatorHash,
 )
import Ledger.Value (CurrencySymbol, Value, mpsSymbol)
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude qualified as P

validateSeedSale :: SeedSaleParams -> SeedSaleDatum -> SeedSaleRedeemer -> Ledger.ScriptContext -> P.Bool
validateSeedSale
  params@SeedSaleParams {pOperatorPKH}
  datum
  redeemer
  ctx@ScriptContext {scriptContextTxInfo} =
    case redeemer of
      Withdraw pkh rAmount -> validateWithdraw params datum ctx rAmount pkh
      Update -> P.traceIfFalse "Invalid operator" (txSignedBy scriptContextTxInfo pOperatorPKH)
      Buy pkh rAmount -> validateBuyGada params datum ctx rAmount pkh

validateWithdraw :: SeedSaleParams -> SeedSaleDatum -> ScriptContext -> P.Integer -> PubKeyHash -> P.Bool
validateWithdraw
  params
  datum@SeedSaleDatum {}
  ctx@ScriptContext {scriptContextTxInfo = info@TxInfo {txInfoValidRange}}
  rAmount
  pkh =
    P.traceIfFalse "Invalid person" (txSignedBy info pkh)
      P.&& P.traceIfFalse "Invalid amount" (rAmount P.> 0)

validateBuyGada :: SeedSaleParams -> SeedSaleDatum -> ScriptContext -> P.Integer -> PubKeyHash -> P.Bool
validateBuyGada
  params
  datum@SeedSaleDatum {}
  ctx@ScriptContext {scriptContextTxInfo = info@TxInfo {txInfoValidRange}}
  rAmount
  pkh =
    P.traceIfFalse "Invalid person" (txSignedBy info pkh)
      P.&& P.traceIfFalse "Invalid amount" (rAmount P.> 0)

seedSaleScript :: SeedSaleParams -> TypedValidator SeedSalePosition
seedSaleScript =
  mkTypedValidatorParam @SeedSalePosition
    $$(compile [||validateSeedSale||])
    $$(compile [||mkUntypedValidator||])

seedSaleAddress :: SeedSaleParams -> Address
seedSaleAddress = (`scriptValidatorHashAddress` P.Nothing) P.. validatorHash P.. seedSaleScript

validateSeedSaleAuthToken :: SeedSaleAuthTokenParams -> () -> ScriptContext -> P.Bool
validateSeedSaleAuthToken SeedSaleAuthTokenParams {tpOperatorPKH} _ ScriptContext {scriptContextTxInfo} =
  P.traceIfFalse "Invalid operator" (txSignedBy scriptContextTxInfo tpOperatorPKH)

seedSaleAuthTokenPolicy :: SeedSaleAuthTokenParams -> MintingPolicy
seedSaleAuthTokenPolicy params =
  mkMintingPolicyScript P.$
    $$(compile [||mkUntypedMintingPolicy P.. validateSeedSaleAuthToken||]) `applyCode` liftCode params

seedSaleAuthTokenCurrencySymbol :: SeedSaleAuthTokenParams -> CurrencySymbol
seedSaleAuthTokenCurrencySymbol = mpsSymbol P.. mintingPolicyHash P.. seedSaleAuthTokenPolicy
