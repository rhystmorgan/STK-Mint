module Common.Random where 

import System.Random
import Data.List 
import Common.NFTs


-- Unused - Token info in NFTs.hs

-- nfts :: [(String, String)]
-- nfts = [("STK0001","STK0001.json"),("STK0002","STK0002.json"),("STK0003","STK0003.json"),("STK0004","STK0004.json"),("STK0005","STK0005.json")]

randomVal :: [(a, a)] -> Int
randomVal xs = fst $ random (mkStdGen (length xs))

randomIndex :: [(a, a)] -> Int
randomIndex xs = randomVal xs `mod` (length xs)

shuffle :: RandomGen t => t -> [a] -> [a]
shuffle g [] = [] 
shuffle g xs = rElem : shuffle newGen newList
  where 
   rTuple   = randomR (0,(length xs) - 1) g
   rIndex   = fst rTuple
   newGen   = snd rTuple
   rElem    = xs !! rIndex
   newList  = take rIndex xs ++ drop (rIndex+1) xs

cutList :: Int -> [a] -> ([a], [a])
cutList i xs = splitAt i xs

concList :: ([a], [a]) -> [a]
concList (ys, zs) = (reverse zs) ++ ys

configMint :: [(a, a)] -> [(a, a)]
configMint xs = concList (cutList (randomIndex xs) (shuffle (mkStdGen (length xs)) xs))

rNFT :: [(a, a)] -> (a, a)
rNFT xs = head (configMint xs)

dropNFT :: [(a, a)] -> [(a, a)]
dropNFT xs = tail (configMint xs)

mintNFT :: [(a, a)] -> a 
mintNFT xs = fst $ rNFT xs

shuffleDatum :: Integer -> [(String, String)] -> String
shuffleDatum n xs
    | n == 0 = mintNFT xs
    | n >= 1 = shuffleDatum (n-1) (dropNFT xs)

-- IO

select :: [(String, String)] -> IO ()
select xs = do
    if xs == [] then do
        putStrLn "No more tokens to mint"
    else do
        mint xs

mint :: [(String, String)] -> IO ()
mint xs = do
    putStrLn $ show (mintNFT xs)
    -- putStrLn $ show (configMint xs) -- For verification of reordering list
    -- putStrLn $ show (tail (configMint xs)) to confirm dropping head of the list (adjusting state)
    select (dropNFT xs)

singleMint :: [(String, String)] -> IO ()
singleMint xs = do 
    putStrLn $ show (mintNFT xs)