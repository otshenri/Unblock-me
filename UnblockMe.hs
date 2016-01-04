-- Henri & Thomas
--
-- Unblock Me
--
-- Programmeerimiskeeled
--
--
import Data.List
import Data.Char
import System.IO
import Data.Hashable
import Data.String
import System.IO.Unsafe
--import Data.Sequence

{-}

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

--Sõne mängulauaks ja vastupidi
showT :: Table -> String
showT tabel = 
  {-let listike = ["bar", "bar", "bar"]
      sona = "foo"
  in show(Data.List.length(update 2 sona $ fromList listike))-}
  --show(x) ++ show(y) ++ show(x2) ++ show (y2) ++ show(blokid)
  let read = addBlocksToStringList tabel (createStringList tabel) 
  in intercalate "\n" read

createStringList:: Table -> [String]
createStringList (T (x,y) (x2, y2) blokid) = 
  let esimeneRida = Data.List.take (x+1) (repeat '#')
      trell = '#'
      tyhikud = Data.List.take (x-1) (repeat ' ')
  in let readTyhikuni = Data.List.take (y2-1) (repeat (trell: tyhikud ++ [trell]))
         tyhikuRida = trell: tyhikud ++ [' ']
         readTyhikust = Data.List.take ((y-2)-(y2-1)) (repeat (trell: tyhikud ++ [trell]))
     in esimeneRida : (readTyhikuni ++ [tyhikuRida] ++ readTyhikust) ++ [esimeneRida]
  
addBlocksToStringList:: Table -> [String] -> [String]
addBlocksToStringList (T (x,y) (x2, y2) []) sisu = sisu
addBlocksToStringList (T (x,y) (x2, y2) (t:ts)) sisu = 
  let number = fst t
      punktiList = snd t
  in addBlocksToStringList (T (x,y) (x2, y2) ts)(addOneBlock number punktiList sisu)
  
addOneBlock:: Int -> [Point] -> [String] -> [String]
addOneBlock _ [] listike = listike
addOneBlock number (x:xs) listike =
  let xCordinate = fst x
      yCordinate = snd x
  in let uus = take xCordinate (listike !! yCordinate) ++ [(show(number) !! 0)] ++ drop (xCordinate + 1) (listike !! yCordinate)
     in addOneBlock number xs (take yCordinate listike ++ [uus] ++ drop (yCordinate + 1) listike)
  --update x (show(number) !! 0) ( fromList (listike !! y))
  --update y (muudetav) ( fromList listike)

readT :: String -> Maybe Table
readT sisu =
  let suurus = getSize sisu
      auk = getHole 0 (lines sisu)
      blokid = getBlocks 0 sisu
  in Just(T suurus auk blokid)

getSize:: String -> Point
getSize sisu =
  let all@x:xs = lines sisu
  in let lauakorgus = (Data.List.length all)-1
         laualaius = (Data.List.length x)-1
     in (laualaius, lauakorgus)

getHole:: Int -> [String] -> Point
getHole reanumber (x:xs) =
  if last x == ' '
    then 
    let xCordinate = (Data.List.length x) -1
        yCordinate = reanumber
    in (xCordinate, yCordinate)
    else getHole (reanumber+1) xs

getBlocks:: Int -> String -> [(Int,[Point])]
getBlocks n sisu =
  if fst (getTuple n (lines sisu)) == -1 -- kui tuple esimene element on -1 siis järelikult numbrid otsas ja lõpetame
    then []
    else getTuple n (lines sisu) : getBlocks (n+1) sisu
  

getTuple:: Int -> [String] -> (Int,[Point])
getTuple number sisu = 
  if getPointArray (0,0) number sisu /= [] -- 
    then (number, (getPointArray (0,0) number sisu))
    else (-1, []) -- panen -1 tuple esimeseks liikmeks kui selle numbriga blokke enam ei leidnud

getPointArray:: Point -> Int -> [String] -> [Point]
getPointArray _ _ [] = []
getPointArray (x,y) number (t:ts) =
  getPointsFromSingleRow (x,y) number t ++ getPointArray (x, y+1) number ts
  
getPointsFromSingleRow:: Point -> Int -> [Char] -> [Point]
getPointsFromSingleRow _ _ [] = []
getPointsFromSingleRow (x,y) number (t:ts) = 
  if [t] == show(number)
    then (x,y) : getPointsFromSingleRow (x+1,y) number ts
    else getPointsFromSingleRow (x+1,y) number ts

main = do  
  contents <- readFile "laud.txt"
  let Just(tabel) = readT contents
  let tabel2 = T (1,2) (3,4) [(5,[(6,7)])]
  putStrLn (showT tabel)






 
