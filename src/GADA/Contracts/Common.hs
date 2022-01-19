{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Common seed sale functions and values.
module GADA.Contracts.Common where

import GADA.Contracts.Types
import Ledger.Ada (lovelaceValueOf)
import Ledger.Time (POSIXTime (getPOSIXTime))
import Ledger.Value (Value, assetClassValue)
import PlutusTx.Prelude (Integer, divide, min, (*), (-), (.), (<>))

-- | The minimum amount of Lovelace required at a seed sale output.
--
-- It is 2_000_000 (2 ADA) at the moment.
minLovelaceInSeedSaleOutput :: Integer
minLovelaceInSeedSaleOutput = 2_000_000

-- | The minimum amount of ADA required at a seed sale output.
--
-- It is 2 ADA at the moment.
minADAInSeedSaleOutput :: Value
minADAInSeedSaleOutput = lovelaceValueOf minLovelaceInSeedSaleOutput

-- | How long is an epoch in seconds.
--
-- It is 5 * 24 * 60 * 60 = 432_000 at the moment.
--
-- One can build with the short-epoch cabal flag to make it 2 seconds,
-- which is useful for testing and simulation.
-- anEpochInSec :: Integer
-- #ifdef SHORT_EPOCH
-- anEpochInSec = 2
-- #else
-- anEpochInSec = 5 * 24 * 60 * 60
-- #endif

-- | How long is an epoch in milliseconds.
--
-- It is `anEpochInSec` * 1_000 = 432_000_000 at the moment.
-- anEpochInMs :: Integer
-- anEpochInMs = anEpochInSec * 1_000

-- | `unlockedAmount` @datum withdrawalTime@ calculates
-- how much has been unlocked from a seedSale position at the given withdrawal time.
-- unlockedAmount :: SeedSaleDatum -> POSIXTime -> Integer
-- unlockedAmount SeedSaleDatum {dTotalEpochs, dAmountPerEpoch, dStartTime} withdrawalTime =
--   let lastedEpochs = getPOSIXTime (withdrawalTime - dStartTime) `divide` anEpochInMs
--    in dAmountPerEpoch * min lastedEpochs dTotalEpochs

-- | `seedSaleValue` @params seedSaleAmount@
-- constructs the value of a seedSale position from its amount. Including:
--
-- - `pSeedSaleAsset`: The passed-in seedSale amount of this output.
-- - `pAuthToken`: Exactly one auth token.
-- - `ADA`: `minADAInSeedSaleOutput`.
-- seedSaleValue :: SeedSaleParams -> Integer -> Value
-- seedSaleValue SeedSaleParams {pAuthToken, pSeedSaleAsset} =
--   ((minADAInSeedSaleOutput <> assetClassValue pAuthToken 1) <>) . assetClassValue pSeedSaleAsset
