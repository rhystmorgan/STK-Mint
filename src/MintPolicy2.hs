{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MintPolicy2 where

import           Control.Monad hiding (fmap)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Map as Map 
import           Data.Text (Text)
import           Data.Void (Void)
import           GHC.Generics (Generic)

import           Plutus.Contract as Contract
import           Plutus.Trace.Emulator as Emulator

import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import qualified PlutusTx.Builtins as Builtins
import qualified Plutus.V1.Ledger.Scripts as Plutus 

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import Ledger.Address 
import Plutus.V1.Ledger.Time 
import Plutus.V1.Ledger.Scripts

import           Ledger hiding (mint, singleton)
import           Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts -- Plutus.Script.Utils.V2.Scripts
import           Ledger.Value as Value
import           Ledger.Ada           as Ada

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, ToArgument)
import           Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types (KnownCurrency (..))

import           Prelude (IO, Show (..), String, (<>))

import           Text.Printf (printf)
import           Wallet.Emulator.Wallet 

import qualified Common.Utils as U --import Utils module from the common dir
import qualified Common.Random as Random 
import           Common.NFTs as NFTs
import           RequestMint as Req
import           PlutusTx.Builtins.Class as Class

data MintDatum = MintDatum 
                { metadata :: !BuiltinByteString
                , count :: !Integer
                , address :: !PubKeyHash
                , cost :: !Integer
                } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''MintDatum [('MintDatum, 0)]



mkTokenPolicy :: () -> ScriptContext -> Bool
mkTokenPolicy _ ctx = validate -- Change to Redeemer == Datum Hash && must contain Validator in TxOut
    where 
        validate :: Bool
        validate = --True
            -- containsValidator && 
            -- validateRecipients &&
            validateTxOuts -- currently only validates txOut is to A SCRIPT not anything in particular

        validateTxOuts :: Bool
        validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

        txOutValidate :: TxOut -> Bool
        txOutValidate txo = isPayToScriptOut txo

mintPolicy :: Scripts.MintingPolicy
mintPolicy = mkMintingPolicyScript
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkTokenPolicy ||])
    --`PlutusTx.applyCode`
    --PlutusTx.liftCode dat

plutusScript :: Script 
plutusScript = unMintingPolicyScript mintPolicy 

validator :: Validator 
validator = Validator plutusScript 

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol mintPolicy 

mintAddress :: Ledger.Address 
mintAddress = Ledger.scriptAddress MintPolicy2.validator
