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

import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import           Ledger hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts 
import           Prelude (IO, Show (..), String, (<>))

import qualified Common.Utils as U 
import           ThreadToken as Thread 

----------------------------
-- MintingPolicy On-Chain --
----------------------------

data MintDatum = MintDatum 
                { metadata :: !BuiltinByteString
                , count :: !Integer
                , address :: !PubKeyHash
                , cost :: !Integer
                } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''MintDatum [('MintDatum, 0)]

data MintInfo = MintInfo
                { tSymbol :: !CurrencySymbol
                } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''MintInfo 
PlutusTx.makeIsDataIndexed ''MintInfo [('MintInfo, 0)]

mintInfo :: MintInfo
mintInfo = MintInfo {tSymbol = Thread.threadSymbol}

mkTokenPolicy :: MintInfo -> () -> ScriptContext -> Bool
mkTokenPolicy mintInfo _ ctx = traceIfFalse "MintValidation Failed" (validate ctx) 
    where 
        validate :: ScriptContext -> Bool
        validate ctx = 
            validateTxOuts -- currently only validates txOut is to A SCRIPT & Contains Thread Token

        validateTxOuts :: Bool
        validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

        txOutValidate :: TxOut -> Bool
        txOutValidate txo = 
            isPayToScriptOut txo &&
            checkOutput txo

        checkOutput :: TxOut -> Bool
        checkOutput txo = let   flatValue = flattenValue (txOutValue txo) in 
                                valueOf (txOutValue txo) (tSymbol mintInfo) (TokenName "Thread") == 1 

mintPolicy :: Scripts.MintingPolicy
mintPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mintInfo
    where
        wrap mintInfo = Scripts.wrapMintingPolicy $ mkTokenPolicy mintInfo

plutusScript :: Script 
plutusScript = unMintingPolicyScript mintPolicy 

validator :: Validator 
validator = Validator plutusScript 

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol mintPolicy 

mintAddress :: Ledger.Address 
mintAddress = Ledger.scriptAddress MintPolicy2.validator
