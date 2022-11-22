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

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import Ledger.Address 

import           Ledger hiding (mint, singleton)
import           Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts 
import           Ledger.Value as Value
import           Ledger.Ada           as Ada

import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, ToArgument)
import           Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types (KnownCurrency (..))

import           Prelude (IO, Show (..), String, (<>))

import           Text.Printf (printf)
import           Wallet.Emulator.Wallet 

import qualified Common.Random as Random 
import           Common.NFTs as NFTs
import           RequestMint as Req
import           MintPolicy2 as Mint
import           ThreadToken as Thread
import           LockingContract as Lock
import           PlutusTx.Builtins.Class as Class

---------------------------------
-- Contract Schema & Endpoints --
---------------------------------

type RequestSchema = 
    Endpoint "request" RequestParams

data RequestParams = RP 
    { rpAddress :: PubKeyHash
    , rpTreasury :: PaymentPubKeyHash 
    } deriving (Generic, FromJSON, ToJSON, ToSchema)

type ThreadSchema =
    Endpoint "thread" ThreadParams

data ThreadParams = TP 
    { tpAddress :: PubKeyHash
    , tpRequestSc :: Validator
    } deriving (Generic, FromJSON, ToJSON)

--------------------------------------
-- Helper Functions For Script Info -- 
--------------------------------------

reqAddress :: Ledger.Address
reqAddress = Req.requestAddress $ ContractInfo{lockingSc = Lock.lockingAddress}

reqHash :: Ledger.ValidatorHash
reqHash = Scripts.validatorHash $ reqTypedValidator

reqTypedValidator :: Scripts.TypedValidator Request
reqTypedValidator = Req.requestPolicy ContractInfo{lockingSc = Lock.lockingAddress} 

reqVal :: Validator 
reqVal = Scripts.validatorScript reqTypedValidator

mintHash :: MintingPolicyHash
mintHash = mintingPolicyHash $ Mint.mintPolicy 

updateMetadata :: Integer -> BuiltinByteString
updateMetadata x = Class.stringToBuiltinByteString (Random.shuffleDatum x NFTs.nfts)

getTokenNameRef :: Integer -> BuiltinByteString
getTokenNameRef x = Class.stringToBuiltinByteString ("(100)StakingDAO NFT #" ++ integerToString x)

getTokenNameNft :: Integer -> BuiltinByteString
getTokenNameNft x = Class.stringToBuiltinByteString ("(222)StakingDAO NFT #" ++ integerToString x)

integerToString :: Integer -> String
integerToString x = show $ x

---------------------
-- Off-Chain Logic -- 
---------------------

thread :: ThreadParams -> Contract w ThreadSchema Text ()
thread (TP tpAddress tpRequestSc) = do 
    utxos <- utxosAt reqAddress
    case Map.toList utxos of
        []      -> do
    -- mint token and send to ReqAddress with Datum
            let requestDatum = Req.RequestDatum 
                            { Req.metadata = updateMetadata 0 -- updateMetadata 0
                            , Req.count = (-1)
                            , Req.address = tpAddress 
                            , Req.cost = 40
                            }
                tkn         = Value.singleton (Thread.threadSymbol) (TokenName "Thread") 1
                
                lookups     = Constraints.mintingPolicy Thread.threadPolicy <> 
                              Constraints.otherScript reqVal 
                
                tx          = Constraints.mustMintValue tkn <>
                              Constraints.mustPayToOtherScript reqHash (Datum $ PlutusTx.toBuiltinData requestDatum) ((Ada.lovelaceValueOf 2000000) <> tkn)
            
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx 
            logInfo @String $ printf "Submitted mint request of %s" (show $ tkn)
            logInfo @String $ printf "available UTxOs at ScriptAddress %s" (show $ utxos)
        
        (oref, a):xs -> error ()

request :: RequestParams -> Contract w RequestSchema Text ()
request (RP rpAddress rpTreasury) = do
    utxos <- Map.filter f <$> utxosAt reqAddress
    lockd <- utxosAt lockingAddress 
    case Map.toList utxos of
        []              -> error () -- no utxos

        (oref, a):xs -> do -- utxos
            let requestDatum = newDatum (oref, a)
                
                lockingDatum = Lock.LD
                    { metadata = (Req.metadata requestDatum)
                    , count = (Req.count requestDatum)
                    , address = (Req.address requestDatum)
                    , cost = (Req.cost requestDatum)
                    , beneficiary = rpTreasury
                    , deadline = 1596059101000 }

                recip   = PaymentPubKeyHash rpAddress
                
                nft     = Value.singleton (Mint.curSymbol) (TokenName $ getTokenNameNft $ (Req.count requestDatum) + 1) 1
                ref     = Value.singleton (Mint.curSymbol) (TokenName $ getTokenNameRef $ (Req.count requestDatum) + 1) 1 
                tkn     = Value.singleton (Thread.threadSymbol) (TokenName "Thread") 1

                lookups = Constraints.mintingPolicy Mint.mintPolicy <> 
                          Constraints.typedValidatorLookups reqTypedValidator <>
                          Constraints.otherScript reqVal <>
                          Constraints.unspentOutputs utxos
                          
                tx      = Constraints.mustPayWithDatumToPubKey rpTreasury (Datum $ PlutusTx.toBuiltinData requestDatum) ((Ada.lovelaceValueOf 36000000)) <>
                          Constraints.mustPayToTheScript requestDatum ((Ada.lovelaceValueOf 2000000) <> tkn) <>
                          Constraints.mustSpendScriptOutput oref (Redeemer $ Builtins.mkI $ Req.count requestDatum) <>
                          Constraints.mustPayToOtherScript Lock.lockingHash (Datum $ PlutusTx.toBuiltinData lockingDatum) ((Ada.lovelaceValueOf 2000000) <> ref){-(Redeemer $ PlutusTx.toBuiltinData ())-} <>
                          Constraints.mustMintValue (nft <> ref) <>
                          Constraints.mustPayToPubKey recip ((Ada.lovelaceValueOf 2000000) <> nft)
            
            ledgerTx <- submitTxConstraintsWith @Request lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx 
            logInfo @String $ printf "Submitted mint request of %s" (show $ Req.metadata requestDatum)
            logInfo @String $ printf "available UTxOs at ScriptAddress %s" (show $ utxos)
            logInfo @String $ printf "UTXOs at LockingAddress %s" (show $ lockd)
            
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
                            { Req.metadata = updateMetadata ((Req.count d) + 1)  
                            , Req.count = (Req.count d) + 1
                            , Req.address = rpAddress 
                            , Req.cost = 40
                            }
        
        f :: ChainIndexTxOut -> Bool
        f o = assetClassValueOf (_ciTxOutValue $ o) (assetClass Thread.threadSymbol (TokenName "Thread")) == 1

------------------------------------
-- Endpoint Calls & Emulator Info --
------------------------------------

endpoints :: Contract () RequestSchema Text ()
endpoints = awaitPromise request' >> endpoints
    where 
        request' = endpoint @"request" request

endpointz :: Contract () ThreadSchema Text ()
endpointz = awaitPromise thread' >> endpointz
    where
        thread' = endpoint @"thread" thread



mkSchemaDefinitions ''RequestSchema
mkSchemaDefinitions ''ThreadSchema

mkKnownCurrencies []



-----------
-- TRACE --
-----------

testThread :: IO ()
testThread = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpointz
    h2 <- activateContractWallet (knownWallet 2) endpointz
    callEndpoint @"thread" h1 $ TP 
                                { tpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 1)
                                , tpRequestSc = Thread.threadValidator
                                }
    void $ Emulator.waitNSlots 10

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (knownWallet 1) endpointz
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    h4 <- activateContractWallet (knownWallet 4) endpoints 
    h5 <- activateContractWallet (knownWallet 5) endpoints
    h6 <- activateContractWallet (knownWallet 6) endpoints 
    h7 <- activateContractWallet (knownWallet 7) endpoints
    h8 <- activateContractWallet (knownWallet 8) endpoints 
    callEndpoint @"thread" h1 $ TP 
                                { tpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 1)
                                , tpRequestSc = Thread.threadValidator
                                }
    void $ Emulator.waitNSlots 10
    callEndpoint @"request" h2 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 2)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 10
    callEndpoint @"request" h3 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 3)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h4 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 4)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h5 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 5)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h6 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 6)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h7 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 7)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h8 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 8)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h2 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 2)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 10
    callEndpoint @"request" h3 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 3)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h4 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 4)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h5 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 5)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h6 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 6)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h7 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 7)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
    callEndpoint @"request" h8 $ RP 
                                { rpAddress = unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash (knownWallet 8)
                                , rpTreasury = mockWalletPaymentPubKeyHash (knownWallet 1) 
                                }
    void $ Emulator.waitNSlots 2
