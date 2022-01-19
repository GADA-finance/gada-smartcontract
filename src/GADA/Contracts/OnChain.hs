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
  ScriptContext (ScriptContext),
  TxInfo (TxInfo),
  ivFrom,
  mintingPolicyHash,
  mkMintingPolicyScript,
  scriptAddress,
  scriptContextTxInfo,
  txInfoValidRange,
  txSignedBy,
 )
import Ledger.Constraints.OnChain (checkOwnOutputConstraint)
import Ledger.Constraints.TxConstraints (OutputConstraint (OutputConstraint))
import Ledger.Typed.Scripts (
  MintingPolicy,
  TypedValidator,
  mkTypedValidatorParam,
  validatorScript,
  wrapMintingPolicy,
  wrapValidator,
 )
import Ledger.Value (CurrencySymbol, Value, mpsSymbol)
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude qualified as P

-- | `validateSeedSale` @params datum redeemer ctx@
-- validates the transaction that spends the seedSale position output.
validateSeedSale :: SeedSaleParams -> SeedSaleDatum -> SeedSaleRedeemer -> Ledger.ScriptContext -> P.Bool
validateSeedSale
  params
  datum
  redeemer
  ctx@ScriptContext {scriptContextTxInfo} = P.True
    -- case redeemer of
    --   Withdraw rAmount -> validateWithdraw params datum ctx rAmount
    --   Update -> P.traceIfFalse "Invalid operator" (txSignedBy scriptContextTxInfo pOperatorPKH)

-- | `validateWithdraw` @params datum ctx withdrawalAmount@
-- validates the transaction that withdraws from the seedSale position output.
-- validateWithdraw :: SeedSaleParams -> SeedSaleDatum -> ScriptContext -> P.Integer -> P.Bool
-- validateWithdraw
--   params
--   datum@SeedSaleDatum {dSeedSalePKH, dAmountPerEpoch, dTotalEpochs, dWithdrawnAmount}
--   ctx@ScriptContext {scriptContextTxInfo = info@TxInfo {txInfoValidRange}}
--   rAmount =
--     P.traceIfFalse "Invalid person" (txSignedBy info dSeedSalePKH)
--       P.&& P.traceIfFalse "Invalid amount" (rAmount P.> 0 P.&& newWithdrawalAmount P.<= unlockedAmount datum withdrawalTime)
--       P.&& P.traceIfFalse "Invalid output" (checkOwnOutputConstraint ctx (OutputConstraint newDatum newValue))
--     where
--       withdrawalTime :: POSIXTime
--       withdrawalTime = case ivFrom txInfoValidRange of
--         LowerBound (Finite t) _ -> t
--         _ -> P.traceError "Invalid withdrawal time"

--       newWithdrawalAmount :: P.Integer
--       newWithdrawalAmount = dWithdrawnAmount P.+ rAmount

--       newDatum :: SeedSaleDatum
--       newDatum = datum {dWithdrawnAmount = newWithdrawalAmount}

--       newValue :: Value
--       newValue = seedSaleValue params (dTotalEpochs P.* dAmountPerEpoch P.- newWithdrawalAmount)

-- | `seedSaleScript` @params@ compiles the parameterized seedSale script.
seedSaleScript :: SeedSaleParams -> TypedValidator SeedSalePosition
seedSaleScript =
  mkTypedValidatorParam @SeedSalePosition
    $$(compile [||validateSeedSale||])
    $$(compile [||wrapValidator||])

-- | `seedSaleAddress` @params@ produces the address of the parameterized seedSale script.
seedSaleAddress :: SeedSaleParams -> Address
seedSaleAddress = scriptAddress P.. validatorScript P.. seedSaleScript

-- | `validateSeedSaleAuthToken` @params _ ctx@
-- validates the transaction that mints or burns the parameterized seedSale auth token.
validateSeedSaleAuthToken :: SeedSaleAuthTokenParams -> () -> ScriptContext -> P.Bool
validateSeedSaleAuthToken SeedSaleAuthTokenParams {tpOperatorPKH} _ ScriptContext {scriptContextTxInfo} =
  P.traceIfFalse "Invalid operator" (txSignedBy scriptContextTxInfo tpOperatorPKH)

-- | `seedSaleAuthTokenPolicy` @params@
-- compiles the minting policy of the parameterized seedSale auth token.
seedSaleAuthTokenPolicy :: SeedSaleAuthTokenParams -> MintingPolicy
seedSaleAuthTokenPolicy params =
  mkMintingPolicyScript P.$
    $$(compile [||wrapMintingPolicy P.. validateSeedSaleAuthToken||]) `applyCode` liftCode params

-- | `seedSaleAuthTokenCurrencySymbol` @params@
-- produces the currency symbol of the parameterized seedSale auth token.
seedSaleAuthTokenCurrencySymbol :: SeedSaleAuthTokenParams -> CurrencySymbol
seedSaleAuthTokenCurrencySymbol = mpsSymbol P.. mintingPolicyHash P.. seedSaleAuthTokenPolicy
