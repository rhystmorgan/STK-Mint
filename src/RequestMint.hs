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

import           Control.Monad hiding (fmap)
import           Data.Aeson (ToJSON, FromJSON)
import           Data.Map as Map hiding (filter)
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

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types (KnownCurrency (..))

import           Prelude (IO, Show (..), String)

import           Text.Printf (printf)
import           Wallet.Emulator.Wallet 

import qualified Common.Utils as U --import Utils module from the common dir
import qualified Common.Random as Random 
import           Common.NFTs as NFTs
-- import           MintPolicy2 as Mint
import           PlutusTx.Builtins.Class as Class

-- RequestContract On-Chain

data RequestDatum = RequestDatum
    { metadata :: !BuiltinByteString
    , count :: !Integer 
    , address :: !PubKeyHash
    , cost :: !Integer
    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''RequestDatum [('RequestDatum, 0)]

data ContractInfo = ContractInfo 
    { mintingSc :: !Address 
    } deriving (Show, Generic, ToJSON, FromJSON)

-- PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo, 1)]
PlutusTx.makeLift ''ContractInfo

{-# INLINABLE requestValidator #-}
requestValidator :: ContractInfo -> RequestDatum -> Integer -> ScriptContext -> Bool
requestValidator contractInfo@ContractInfo{..} dat _ ctx = validate 
    where 
        validate :: Bool
        validate = 
            validateTxOuts 
        
        validateTxOuts :: Bool
        validateTxOuts = any txOutValidate (txInfoOutputs $ U.info ctx)

        txOutValidate :: TxOut -> Bool
        txOutValidate txo = 
            isToMintingScript txo &&
            checkPayment txo &&
            containsNewDatum txo 

        containsNewDatum :: TxOut -> Bool
        containsNewDatum txo =  
            findDatumHash' (expectedNewDatum $ RequestMint.count dat) (U.info ctx) == txOutDatumHash txo 

        isToMintingScript :: TxOut -> Bool
        isToMintingScript txo = txOutAddress txo == mintingSc

        checkPayment :: TxOut -> Bool
        checkPayment txo = 
            txOutValue txo == (Ada.lovelaceValueOf 40)

        findDatumHash' :: ToData a => a -> TxInfo -> Maybe DatumHash 
        findDatumHash' datum info = findDatumHash (Datum $ toBuiltinData datum) info

        expectedNewDatum :: Integer -> RequestDatum
        expectedNewDatum x = RequestDatum
            { metadata = RequestMint.metadata dat -- updateMetadata x -- onChain cannot handle this shit
            , count = x + 1
            , address = RequestMint.address dat 
            , cost = 40
            }

        -- updateMetadata :: Integer -> BuiltinByteString 
        -- updateMetadata x = Class.stringToBuiltinByteString (Random.shuffleDatum x NFTs.nfts)

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
