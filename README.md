# STK-Mint
Emurgo Project for StakingDAO NFT Minting using CIP-68

## What Does The NFT Mint Do?

Send a payment to the Script and the script mints 2 tokens in line with the CIP-68 specification
1 of which is sent to the treasury address (will be sent to a locking script in the future)

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

## Need To Do -- Completed for V2 along with other improvements

Create vesting contract to be used as a locking script for (100) reference NFTs instead of Treasury (knownWallet 1)
-- Only Accessible by the Treasury Wallet --

Fix the Script Address Tx issue - Consume all UTxOs at RequestScript Address?

Add more Validatiion measures to the Minting Script - currently ONLY checks for a script in the TxOut
