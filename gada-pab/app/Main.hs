{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Run (runWith)

import GADA.PAB.Contracts (Contracts)

main :: IO ()
main = runWith $ Builtin.handleBuiltin @Contracts
