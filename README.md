# STK-Mint
Emurgo Project for StakingDAO NFT Minting using CIP-68

## What Does The NFT Mint Do?

Send a payment to the Request Script which mints 2 tokens in line with the CIP-68 specification

Refernce Token - Prefix (100) - is sent to the Locking Script (only accessible by the Treasury Wallet)

User Token - Prefix (222) - is sent to the user's wallet

The Request Mint script uses a Thread Token, minted by the Treasury Wallet, to hold the count and other metadata in the Datum 

The Minting Script validates that the Tx includes the Thread Token 

## Structure Of Folders

TestEnv - OffChain (includes Trace Emulator)

RequestMint - holds OnChain validation for user Tx

MintPolicy2 - is the minting policy

ThreadToken - Thread Token minting policy

LockingContract - locks Reference Tokens with Datum Metadata accessible by the treasury wallet

TestEnv2 - This was the original Off-Chain for V1 to verify basic processes such as Datum / Random function - Depricated as V2 includes Locking Contract, Thread Token and other improvements

Common:

Utils - Has some Inlinable Functions

Random - Does the sorting to select an NFT to mint

NFTs - List of NFTs (just token Names now for Human readable testing, with IPFS Hash)
