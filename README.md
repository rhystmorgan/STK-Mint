# STK-Mint
Emurgo Project for StakingDAO NFT Minting using CIP-68

## What Does The NFT Mint Do?

Send a payment to the Script and the script mints 2 tokens in line with the CIP-68 specification
1 of which is sent to the treasury address (will be sent to a locking script in the future)

## Structure Of Folders

TestEnv - OffChain (includes Trace Emulator)

RequestMint - holds OnChain validation for user Tx
MintPolicy2 - is the minting policy

Common:
Utils - has some Inlinable Functions
Random - does the sorting to select an NFT to mint
NFTs - List of NFTs (just token Names now for Human readable testing)

## Need To Do

Create vesting contract to be used as a locking script for (100) reference NFTs instead of Treasury (knownWallet 1)
-- Only Accessible by the Treasury Wallet --

Fix the Script Address Tx issue - Consume all UTxOs at RequestScript Address?

Add more Validatiion measures to the Minting Script - currently ONLY checks for a script in the TxOut
