{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Util functions
module GADA.Contracts.Utils where

import Control.Lens (preview, view)
import Ledger.Constraints qualified as Constraints
import Data.Either.Combinators (rightToMaybe)
import Data.List qualified as L
import Data.Map qualified as M
import Ledger (Address, ChainIndexTxOut, TxId (TxId), TxOutRef (TxOutRef, txOutRefId, txOutRefIdx))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OnChain qualified as Constraints
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Contexts qualified as Contexts
import Ledger.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.Contract
import PlutusTx (FromData)
import PlutusTx qualified
import PlutusTx.Prelude qualified as P
import Ledger.Typed.Scripts (DatumType, RedeemerType, TypedValidator, validatorScript, validatorHash)
import Prelude


-- | Data type representing an unique identifier.
type UniqueIdentifier = P.BuiltinByteString

-- | Get an unique identifier from TxOutRef.
-- It uses in total of 34 bytes:
-- * The first 2 bytes are for the output index.
-- * The rest 32 bytes are for the transaction ID.
{-# INLINEABLE genUniqId #-}
genUniqId :: TxOutRef -> UniqueIdentifier
genUniqId TxOutRef {txOutRefId = TxId txId, txOutRefIdx = idx} = aux idx txId
  where
    aux :: P.Integer -> P.BuiltinByteString -> P.BuiltinByteString
    aux num =
      if firstByte P.>= perByte
        then P.error ()
        else P.consByteString firstByte P.. P.consByteString secondByte
      where
        firstByte, secondByte, perByte :: P.Integer
        firstByte = num `P.divide` perByte
        secondByte = num `P.modulo` perByte
        perByte = 256

-- | Make a 'Value' containing only one unit of the given asset class.
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

-- | Get all the value from inputs
{-# INLINEABLE valueWithin #-}
valueWithin :: Ledger.TxInInfo -> Value.Value
valueWithin = Ledger.txOutValue P.. Contexts.txInInfoResolved

{-# INLINEABLE findDatum' #-}
findDatum' :: PlutusTx.FromData a => Ledger.TxInfo -> Scripts.DatumHash -> P.Maybe a
findDatum' info hash = Contexts.findDatum hash info P.>>= PlutusTx.fromBuiltinData P.. Ledger.getDatum

-- | Find the datum of an output
{-# INLINEABLE findDatumFromOutput #-}
findDatumFromOutput :: PlutusTx.FromData a => Ledger.TxInfo -> Ledger.TxOut -> a
findDatumFromOutput info output =
  P.fromMaybe (P.traceError "Invalid datum") (Contexts.txOutDatumHash output P.>>= findDatum' info)

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

-- | Check whether the given txout contains an unit of `asset`
{-# INLINEABLE isAuthenticOutput #-}
isAuthenticOutput :: Value.AssetClass -> Ledger.TxOut -> P.Bool
isAuthenticOutput asset o = hasUnitValue (Ledger.txOutValue o) asset

-- | Find a single authentic output from the current script
{-# INLINEABLE findSingleOutputFromCurrentScript #-}
findSingleOutputFromCurrentScript :: Value.AssetClass -> Ledger.ScriptContext -> Ledger.TxOut
findSingleOutputFromCurrentScript token ctx =
  case P.filter (isAuthenticOutput token) P.$ Ledger.getContinuingOutputs ctx of
    [o] -> o
    _ -> P.traceError "Expect exactly one output"

-- | Find the single authentic output from the other script
{-# INLINEABLE findSingleOutputFromOtherScripts #-}
findSingleOutputFromOtherScripts :: Value.AssetClass -> Ledger.TxInfo -> Ledger.TxOut
findSingleOutputFromOtherScripts token info =
  case P.filter (isAuthenticOutput token) P.$ Contexts.txInfoOutputs info of
    [o] -> o
    _ -> P.traceError "Expect exactly one output"

-- | Find a single output that contains the given token
{-# INLINEABLE findSingleInputWithToken #-}
findSingleInputWithToken :: Value.AssetClass -> Ledger.TxInfo -> Ledger.TxOut
findSingleInputWithToken token info =
  case P.filter (isAuthenticOutput token) P.$ Contexts.txInInfoResolved P.<$> Contexts.txInfoInputs info of
    [o] -> o
    _ -> P.traceError "Expect exactly one input"

-- | Find a single public key hash that signed the transaction,
-- return an error if there are more than one signatures
{-# INLINEABLE findOwnPkh #-}
findOwnPkh :: Ledger.TxInfo -> Ledger.PubKeyHash
findOwnPkh info =
  case Contexts.txInfoSignatories info of
    [x] -> x
    _ -> P.traceError "Expect exactly one signature"

-- | Get the ADA amount of the given ouput
{-# INLINEABLE collateralAmount #-}
collateralAmount :: Ledger.TxOut -> P.Integer
collateralAmount = Ada.getLovelace P.. Ada.fromValue P.. Contexts.txOutValue

-- | Check whether the given output matches an output that returned to the current script
{-# INLINEABLE checkOwnOutput #-}
checkOwnOutput :: PlutusTx.ToData a => Ledger.ScriptContext -> a -> Value.Value -> P.Bool
checkOwnOutput ctx = Constraints.checkOwnOutputConstraint ctx .: Constraints.OutputConstraint

note :: e -> P.Maybe a -> Contract w s e a
note err = P.maybe (throwError err) P.pure

type IsScriptData a = (PlutusTx.ToData (RedeemerType a), PlutusTx.ToData (DatumType a), PlutusTx.FromData (RedeemerType a), PlutusTx.FromData (DatumType a))
type TxPair a = (Constraints.ScriptLookups a, Constraints.TxConstraints (RedeemerType a) (DatumType a))

mustPayToScripts :: IsScriptData a => TypedValidator a -> [(DatumType a, Ledger.Value)] -> TxPair a
mustPayToScripts script dataOutputs = (lookups, tx)
  where
    lookups = Constraints.typedValidatorLookups script
    tx = foldMap (uncurry Constraints.mustPayToTheScript) dataOutputs

-- | The constraint to spend script outputs.
mustSpendScriptOutputs :: PlutusTx.ToData (RedeemerType a) => TypedValidator a -> [(Contexts.TxOutRef, Ledger.ChainIndexTxOut, RedeemerType a)] -> TxPair b
mustSpendScriptOutputs script dataInputs = (lookups, tx)
  where
    unspent = M.fromList $ (\(ref, txInf, _) -> (ref, txInf)) <$> dataInputs
    lookups = Constraints.otherScript (validatorScript script) <> Constraints.unspentOutputs unspent
    tx = foldMap (\(ref, _, redeemer) -> Constraints.mustSpendScriptOutput ref (Ledger.Redeemer $ PlutusTx.toBuiltinData redeemer)) dataInputs

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
submitTxPairs pairs = mkTxConstraints lookups constraints >>= submitTxConfirmed . Constraints.adjustUnbalancedTx
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

{-# INLINABLE (.:) #-}
infixr 8 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f P.$ g x y