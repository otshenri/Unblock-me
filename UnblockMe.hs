-- Henri & Thomas
--
-- Unblock Me
--
-- Programmeerimiskeeled
--
--
import Data.List
import System.IO
import Data.Hashable
import Data.String
import System.IO.Unsafe 



{-}
--Sõne mängulauaks ja vastupidi
readT :: Point -> String -> Maybe Table
showT :: Table -> String

--Nihutused
data PMove = H Int | V Int 
type Move  = (Int, PMove)
moveOne ::  Move  -> Table -> Maybe Table
move    :: [Move] -> Table -> Maybe Table

--Kas andmestruktuur on õige ja võidutingimus täidetud
isValidTable :: Table -> Bool
winCond      :: Table -> Bool

--Võimalikud liigutsed
validMoves :: Table -> [Move]

--Mängupuu kõikide liigutustega
data GameTree = Win Table | Moves Table [(Move, GameTree)] 

--Läbitakse mängupuu laiuti kuni leitakse võitev seisund
mkGameTree :: Table -> GameTree

--Arvutatakse järgnev mängupuude tase
--Siin võetakse list mängupuid ja tagastatakse list kus igas sisendis olevas mängupuus on tehtud üks samm.
--Teiste sõnadega, võtab mängupuudes järgmise taseme
bfsGameTreeStep :: [(GameTree,[Move])] -> [(GameTree,[Move])]

--Korduste väljafiltreerimine
type HashTable k v = H.CuckooHashTable k v

bfsGameTree :: HashTable GameTree [Move] -> [(GameTree,[Move])] -> IO [Move]

--Lahendamine
solve :: Table -> IO [Move]
solve l = do
 ht <- H.new 
 bfsGameTree ht [(mkGameTree l ,[])]
-}



type Point = (Int, Int)
data Table = T Point Point [(Int,[Point])]


main = do  
	contents <- readFile "laud.txt"
	let linesOfContent = lines contents
	return linesOfContent






 
