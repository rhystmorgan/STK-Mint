# Revision history for StkMint

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
TestEnv2 was the first iteration of this project, without a thread token, minting based on the first of a lit of UTxOs at the Request address

I also had the reference NFTs going to the Treasury Address (knownWallet 1)

This would not work in practice because the Treasury would have all of the ADA and Ref Token on thesame UTxO and so the ADA couldnt be spent or transferred without effectively destroying the NFT reference token (Datum)

## 0.2.0.0 -- 2022-11-22

Version 2 has had some major improvements that enable the contracts / minting process to work effectively.

I have implemented a Locking Script (Vesting Contract) to recieve the Reference Tokens (100)... with a ~min UTxO value of 2 ADA
UTxOs at the Locking Script will only be accessible by the Treasury Wallet as specified in its Datum

I have also implemented the Thread Token mechanism to track the count & metadata information for the random functions of the NFT mint.
This means that the tokens are only minted if the Thread Token is present in the transaction, and the next to mint metadata is adjusted based on the information at the datum of that Thread Token, meaning we do not get any repeat mints.

I have also made sure the MintingPolicy only mints if the Tx contains the Thread Token.

## 0.2.0.1 -- TBD

To improve on this Minting mechanism, I would put a timelimit of using the ThreadToken Script, this would prevent more Thread Tokens being minted in the future.

As it stands right now I don't have the time to implement this.

I would also put more checks in the onchain validation of Mint Request, I would ensure the TxOuts include both the Treasury address and the Locking Script to make sure people werent trying to spam the Request Validation or exploit it.

I would also have liked to implement a metadata change in the Emulator test, simply using the treasury address to adjust the metadata on a given Reference Token in the Locking Script, again if I had more time to work on this I thnk it would be a good example of how metadata can be adjusted for NFTs using the CIP-68 Standard.