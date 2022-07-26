{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Util functions
module GADA.Contracts.Utils where

import Control.Lens (preview, view)
import Data.Either.Combinators (rightToMaybe)
import Data.List qualified as L
import Data.Map qualified as M
import Ledger (Address, ChainIndexTxOut, TxId (TxId), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OnChain qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (DatumType, RedeemerType, TypedValidator, validatorHash, mkUntypedValidator,validatorScript)
import Ledger.Value qualified as Value
import Plutus.Contract
import PlutusTx (FromData)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Prelude

type UniqueIdentifier = P.BuiltinByteString

{-# INLINEABLE unitValue #-}
unitValue :: Value.AssetClass -> Value.Value
unitValue ac = Value.assetClassValue ac 1

-- | Check whether the given value contains one unit of the given asset class.
{-# INLINEABLE hasUnitValue #-}
hasUnitValue :: Value.Value -> Value.AssetClass -> P.Bool
hasUnitValue v ac = Value.assetClassValueOf v ac P.== 1

-- | Check whether the given value does not contain the given asset class.
{-# INLINEABLE hasNoneValue #-}
hasNoneValue :: Value.Value -> Value.AssetClass -> P.Bool
hasNoneValue v ac = Value.assetClassValueOf v ac P.== 0

-- | Get the value of the given ChainIndexTxOut
getValue :: Ledger.ChainIndexTxOut -> Ledger.Value
getValue = view Ledger.ciTxOutValue

-- | Get the datum of the given ChainIndexTxOut
getDatum :: PlutusTx.FromData b => Ledger.ChainIndexTxOut -> P.Maybe b
getDatum o = preview Ledger.ciTxOutDatum o P.>>= rightToMaybe P.>>= (PlutusTx.fromBuiltinData P.. Ledger.getDatum)

-- | Convert the DatumType to Scripts.Datum
{-# INLINEABLE toDatum #-}
toDatum :: PlutusTx.ToData a => a -> Scripts.Datum
toDatum = Scripts.Datum P.. PlutusTx.toBuiltinData

-- | Return the first output getting from an address that satisfies the given condition
findFirstOutput ::
  (AsContractError e, FromData datum) =>
  Address ->
  ((TxOutRef, ChainIndexTxOut) -> P.Bool) ->
  Contract w s e (P.Maybe (TxOutRef, ChainIndexTxOut, datum))
findFirstOutput address condition = do
  utxos <- utxosAt address
  P.pure P.$ do
    (oref, o) <- L.find condition (M.toList utxos)
    d <- getDatum o
    P.pure (oref, o, d)

type IsScriptData a = (PlutusTx.ToData (RedeemerType a), PlutusTx.ToData (DatumType a), PlutusTx.FromData (RedeemerType a), PlutusTx.FromData (DatumType a))
type TxPair a = (Constraints.ScriptLookups a, Constraints.TxConstraints (RedeemerType a) (DatumType a))

mustPayToScripts :: IsScriptData a => TypedValidator a -> [(DatumType a, Ledger.Value)] -> TxPair a
mustPayToScripts script dataOutputs = (lookups, tx)
  where
    lookups = Constraints.typedValidatorLookups script
    tx = foldMap (uncurry Constraints.mustPayToTheScript) dataOutputs

-- | The constraint to spend script outputs.
-- | The constraint to spend script outputs.
mustSpendScriptOutputs ::
  PlutusTx.ToData (RedeemerType a) =>
  TypedValidator a ->
  [(TxOutRef, ChainIndexTxOut, RedeemerType a)] ->
  TxPair b
mustSpendScriptOutputs script dataInputs = (lookups, tx)
  where
    unspent = M.fromList $ (\(oref, o, _) -> (oref, o)) <$> dataInputs
    lookups = Constraints.otherScript (validatorScript script) <> Constraints.unspentOutputs unspent
    tx = foldMap perInput dataInputs
    perInput (oref, _, rd) = Constraints.mustSpendScriptOutput oref (Ledger.Redeemer $ PlutusTx.toBuiltinData rd)

mustValidateIn :: Ledger.POSIXTimeRange -> TxPair a
mustValidateIn range = (mempty, Constraints.mustValidateIn range)

-- | Constraint to pay the outputs (include datum and value) to another script.
mustPayToOtherScripts :: IsScriptData b => [(TypedValidator b, [(DatumType b, Ledger.Value)])] -> TxPair a
mustPayToOtherScripts otherScripts = (lookups, tx)
  where
    lookups = foldMap (\(validator, _) -> Constraints.otherScript (validatorScript validator)) otherScripts
    tx = foldMap concatConstraints otherScripts
    concatConstraints (validator, outp) =
      foldMap (\(dt, val) -> Constraints.mustPayToOtherScript (validatorHash validator) (Ledger.Datum $ PlutusTx.toBuiltinData dt) val) outp

submitTxPairs :: (AsContractError e, IsScriptData a) => [TxPair a] -> Contract w s e ()
submitTxPairs pairs = mkTxConstraints lookups constraints >>= submitTxConfirmed
  where
    lookups = foldMap fst pairs
    constraints = foldMap snd pairs

mustMintValue :: [Ledger.MintingPolicy] -> Ledger.Value -> TxPair a
mustMintValue policies val = (lookups, tx)
  where
    lookups = mempty {Constraints.slMPS = M.fromList $ (\pl -> (Ledger.mintingPolicyHash pl, pl)) <$> policies}
    tx = Constraints.mustMintValue val

mustMintValueWithRedeemer :: (PlutusTx.ToData b) => Ledger.MintingPolicy -> b -> Ledger.Value -> TxPair a
mustMintValueWithRedeemer policy re val = (lookups, tx)
  where
    lookups = Constraints.mintingPolicy policy
    tx = Constraints.mustMintValueWithRedeemer (Ledger.Redeemer . PlutusTx.toBuiltinData $ re) val

minADAOutput :: Ledger.Value
minADAOutput = Ada.lovelaceValueOf 2000000
