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

module RequestMint where

import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import           Ledger hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Prelude (IO, Show (..), String) 

import qualified Common.Utils as U 

------------------------------
-- RequestContract On-Chain --
------------------------------

data RequestDatum = RequestDatum
    { metadata :: !BuiltinByteString
    , count :: !Integer 
    , address :: !PubKeyHash
    , cost :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''RequestDatum [('RequestDatum, 0)]

data ContractInfo = ContractInfo 
    { lockingSc :: !Address 
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''ContractInfo

{-# INLINABLE requestValidator #-}
requestValidator :: ContractInfo -> RequestDatum -> Integer -> ScriptContext -> Bool
requestValidator contractInfo@ContractInfo{..} dat _ ctx = traceIfFalse "RequestValidation Failed" validate 
    where 
        validate :: Bool
        validate = 
            validateTxOuts 
        
        validateTxOuts :: Bool
        validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

        txOutValidate :: TxOut -> Bool
        txOutValidate txo = 
            -- isToLockingScript txo &&
            checkOutput txo &&
            containsNewDatum txo 

        containsNewDatum :: TxOut -> Bool
        containsNewDatum txo =  
            findDatumHash' (expectedNewDatum $ RequestMint.count dat) (U.info ctx) == txOutDatumHash txo 

        -- isToLockingScript :: TxOut -> Bool
        -- isToLockingScript txo = txOutAddress txo == lockingSc

        checkOutput :: TxOut -> Bool
        checkOutput txo = length (flattenValue $ (txOutValue txo)) == 2

        findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash 
        findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

        expectedNewDatum :: Integer -> RequestDatum
        expectedNewDatum x = RequestDatum
            { metadata = RequestMint.metadata dat 
            , count = x + 1
            , address = RequestMint.address dat 
            , cost = 40
            }

data Request 
instance Scripts.ValidatorTypes Request where 
    type instance DatumType Request = RequestDatum 
    type instance RedeemerType Request = Integer

requestPolicy :: ContractInfo -> Scripts.TypedValidator Request
requestPolicy contractInfo = Scripts.mkTypedValidator @Request  
    ($$(PlutusTx.compile [|| requestValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode contractInfo) 
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @RequestDatum @Integer

validator :: ContractInfo -> Validator
validator = Scripts.validatorScript . requestPolicy

requestAddress :: ContractInfo -> Ledger.Address 
requestAddress = Ledger.scriptAddress . RequestMint.validator 
