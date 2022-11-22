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

module ThreadToken where

import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import           Ledger hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Prelude (IO, Show (..), String)

import           Common.Utils as U

-----------------------------------
-- Thread Token Minting On-Chain --
-----------------------------------

-- data ContractInfo = ContractInfo 
--     { treasury :: !PaymentPubKeyHash 
--     , deadline :: !POSIXTime
--     } deriving (Show, Generic, ToJSON, FromJSON)

-- PlutusTx.makeLift ''ContractInfo

mkThreadPolicy :: () -> ScriptContext -> Bool
mkThreadPolicy _ ctx = validate 
    where
        validate :: Bool
        validate = 
            checkMint
    
        checkMint :: Bool
        checkMint = case flattenValue (txInfoMint $ U.info ctx) of
            [(cs, tn, amt)] -> cs  == ownCurrencySymbol ctx && tn == TokenName "Thread" && amt == 1
            _                -> False

        -- signedByTreasury :: Bool 
        -- signedByTreasury = txSignedBy (U.info ctx)

threadPolicy :: Scripts.MintingPolicy
threadPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) 
    where 
        wrap = Scripts.wrapMintingPolicy mkThreadPolicy 

threadSymbol :: CurrencySymbol
threadSymbol = scriptCurrencySymbol $ threadPolicy

threadScript :: Script 
threadScript = unMintingPolicyScript threadPolicy 

threadValidator :: Validator 
threadValidator = Validator threadScript