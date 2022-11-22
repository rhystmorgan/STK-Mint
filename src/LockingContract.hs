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

module LockingContract where


import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.V1.Ledger.Api
import Ledger.Address 
import           Ledger hiding (mint, singleton)
import qualified Ledger.Typed.Scripts as Scripts 
import           Prelude (IO, Show (..), String)

import           Common.Utils as U

-------------------------------
-- Locking Contract On-Chain --
-------------------------------

data LockingDatum = LD 
                    { metadata :: !BuiltinByteString
                    , count :: !Integer
                    , address  :: !PubKeyHash
                    , cost  :: !Integer
                    , beneficiary :: !PaymentPubKeyHash
                    , deadline :: !POSIXTime
                    } deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''LockingDatum

{-# INLINABLE mkLockingValidator #-}
mkLockingValidator :: LockingDatum -> () -> ScriptContext -> Bool
mkLockingValidator dat () ctx = traceIfFalse "Beneficiary Not Signed" signedByBeneficiary &&
                                traceIfFalse "Deadline Not Reached" deadlineReached
    where

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy (U.info ctx) $ unPaymentPubKeyHash $ beneficiary dat

        deadlineReached :: Bool
        deadlineReached = contains (from $ deadline dat) $ txInfoValidRange $ U.info ctx --anytime from the deadline into the future is acceptable

        -- boiler Plate
data Locking
instance Scripts.ValidatorTypes Locking where
    type instance DatumType Locking = LockingDatum
    type instance RedeemerType Locking = ()

typedValidator :: Scripts.TypedValidator Locking
typedValidator = Scripts.mkTypedValidator @Locking
    $$(PlutusTx.compile [|| mkLockingValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @LockingDatum @()

lockingValidator :: Validator
lockingValidator = Scripts.validatorScript typedValidator 

lockingHash :: Ledger.ValidatorHash 
lockingHash = Scripts.validatorHash typedValidator

lockingAddress :: Ledger.Address
lockingAddress = scriptAddress lockingValidator