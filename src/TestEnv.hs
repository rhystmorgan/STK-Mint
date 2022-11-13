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

module TestEnv where

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
import           MintPolicy2 as Mint

import           PlutusTx.Builtins.Class as Class

--------------------
-- OFF-CHAIN CODE --
--------------------

type RequestSchema = 
    Endpoint "request" RequestParams

data RequestParams = RP 
    { rpAddress :: PubKeyHash
    , rpTreasury :: PaymentPubKeyHash 
    } deriving (Generic, FromJSON, ToJSON, ToSchema)

reqAddress :: Ledger.Address
reqAddress = Req.requestAddress $ ContractInfo{mintingSc = Mint.mintAddress} --True}

reqHash :: Ledger.ValidatorHash
reqHash = Scripts.validatorHash $ reqTypedValidator

reqTypedValidator :: Scripts.TypedValidator Request
reqTypedValidator = Req.requestPolicy ContractInfo{mintingSc = Mint.mintAddress} --True}

reqVal :: Validator 
reqVal = Scripts.validatorScript reqTypedValidator

--mintAddress :: Ledger.Address
--mintAddress = Mint.mintAddress --True

mintHash :: MintingPolicyHash
mintHash = mintingPolicyHash $ Mint.mintPolicy --True

--mintValidator :: Scripts.Validator
--mintValidator = Mint.validator --True

--mintPolicy :: Scripts.MintingPolicy
--mintPolicy = Mint.policy --True

updateMetadata :: Integer -> BuiltinByteString
updateMetadata x = Class.stringToBuiltinByteString (Random.shuffleDatum x NFTs.nfts)

getTokenNameRef :: Integer -> BuiltinByteString
getTokenNameRef x = Class.stringToBuiltinByteString ("(100)StakingDAO NFT #" ++ integerToString x)

getTokenNameNft :: Integer -> BuiltinByteString
getTokenNameNft x = Class.stringToBuiltinByteString ("(222)StakingDAO NFT #" ++ integerToString x)

integerToString :: Integer -> String
integerToString x = show $ x

request :: RequestParams -> Contract w RequestSchema Text ()
request (RP rpAddress rpTreasury) = do
    utxos <- utxosAt reqAddress 
    case Map.toList utxos of
        []              -> do -- no utxos
            let requestDatum = Req.RequestDatum 
                    { Req.metadata = updateMetadata 0 -- updateMetadata 0
                    , Req.count = 0
                    , Req.address = rpAddress 
                    , Req.cost = 40
                    }
                recip   = PaymentPubKeyHash rpAddress
                
                nft     = Value.singleton (Mint.curSymbol) (TokenName $ getTokenNameNft $ (Req.count requestDatum) + 1) 1
                ref     = Value.singleton (Mint.curSymbol) (TokenName $ getTokenNameRef $ (Req.count requestDatum) + 1) 1
                
                lookups = Constraints.mintingPolicy Mint.mintPolicy <> Constraints.typedValidatorLookups reqTypedValidator
                
                tx      = Constraints.mustPayWithDatumToPubKey rpTreasury (Datum $ PlutusTx.toBuiltinData requestDatum) ((Ada.lovelaceValueOf 40000000) <> ref) <>
                          Constraints.mustPayToTheScript requestDatum (Ada.lovelaceValueOf 2000000) <>
                          Constraints.mustMintValue (nft <> ref) <>
                          Constraints.mustPayWithDatumToPubKey recip (Datum $ PlutusTx.toBuiltinData requestDatum) ((Ada.lovelaceValueOf 2000000) <> nft)
            ledgerTx <- submitTxConstraintsWith @Request lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx 
            logInfo @String $ printf "Submitted mint request of %s" (show $ Req.metadata requestDatum)
        (oref, a):xs -> do -- utxos
            let requestDatum = newDatum (oref, a)
                
                recip   = PaymentPubKeyHash rpAddress
                
                nft     = Value.singleton (Mint.curSymbol) (TokenName $ getTokenNameNft $ (Req.count requestDatum) + 1) 1
                ref     = Value.singleton (Mint.curSymbol) (TokenName $ getTokenNameRef $ (Req.count requestDatum) + 1) 1 
                
                lookups = Constraints.mintingPolicy Mint.mintPolicy <> 
                          Constraints.typedValidatorLookups reqTypedValidator <>
                          -- Constraints.otherScript reqVal <>
                          Constraints.unspentOutputs utxos
                          
                tx      = Constraints.mustPayWithDatumToPubKey rpTreasury (Datum $ PlutusTx.toBuiltinData requestDatum) ((Ada.lovelaceValueOf 40000000) <> ref) <>
                          Constraints.mustPayToTheScript requestDatum (Ada.lovelaceValueOf 2000000) <>
                          -- Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ()){-(Redeemer $ PlutusTx.toBuiltinData ())-} <>
                          Constraints.mustMintValue (nft <> ref) <>
                          Constraints.mustPayToPubKey recip ((Ada.lovelaceValueOf 2000000) <> nft)
                          -- Constraints.mustPayWithDatumToPubKey recip (Datum $ PlutusTx.toBuiltinData requestDatum) ((Ada.lovelaceValueOf 2000000) <> nft)
            ledgerTx <- submitTxConstraintsWith @Request lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx 
            logInfo @String $ printf "Submitted mint request of %s" (show $ Req.metadata requestDatum)
            logInfo @String $ printf "available UTxOs at ScriptAddress %s" (show $ utxos)
    where
        newDatum (oref, a) = case _ciTxOutDatum a of 
            Left _      ->  Req.RequestDatum 
                            { Req.metadata = updateMetadata 0 
                            , Req.count = 0
                            , Req.address = rpAddress 
                            , Req.cost = 40
                            }
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of 
                Nothing ->  Req.RequestDatum 
                            { Req.metadata = updateMetadata 0 
                            , Req.count = 0
                            , Req.address = rpAddress 
                            , Req.cost = 40
                            }
                Just d  ->  Req.RequestDatum
                            { Req.metadata = updateMetadata (Req.count d) 
                            , Req.count = (Req.count d) + 1
                            , Req.address = rpAddress 
                            , Req.cost = 40
                            }
        
        -- toListDatum utxos = 

endpoints :: Contract () RequestSchema Text ()
endpoints = awaitPromise request' >> endpoints
    where 
        request' = endpoint @"request" request

mkSchemaDefinitions ''RequestSchema
mkKnownCurrencies []

-----------
-- TRACE --
-----------

-- Emulator trace just starts up the wallets and simulates purchasing of NFTs by multiple wallets

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints 
    h3 <- activateContractWallet (knownWallet 3) endpoints
    h4 <- activateContractWallet (knownWallet 4) endpoints 
    h5 <- activateContractWallet (knownWallet 5) endpoints
    h6 <- activateContractWallet (knownWallet 6) endpoints 
    h7 <- activateContractWallet (knownWallet 7) endpoints
    h8 <- activateContractWallet (knownWallet 8) endpoints 
    callEndpoint @"request" h2 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 2)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20
    callEndpoint @"request" h3 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 3)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20
    callEndpoint @"request" h4 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 4)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20
    callEndpoint @"request" h5 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 5)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20
    callEndpoint @"request" h2 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 2)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20
    callEndpoint @"request" h3 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 3)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20
    callEndpoint @"request" h4 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 4)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20
    callEndpoint @"request" h5 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 5)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 20