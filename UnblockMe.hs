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


-----------------------------------------------------------------------------------------------------------------

readT :: String -> Maybe Table
readT sisu =
  let suurus = getSize (lines sisu)
      auk = getHole 0 (lines sisu)
      blokid = getBlocks 0 sisu
  in Just(T suurus auk blokid)

getSize:: [String] -> Point
getSize (x:xs) =
  let lauakorgus = (Data.List.length xs)
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


-----------------------------------------------------------------------------------------------------------

--for debugging
prindiTabel:: Table -> String
prindiTabel (T (x,y) (x2, y2) blokid) =
  show(x) ++ show(y) ++ show(x2) ++ show(y2) ++ show(blokid)



type Move  = (Int, Char)
move :: Move -> Table -> Maybe Table
move (blokinimi, suund) tabeel@(T (x,y) (x2, y2) (w:ws)) = 
  case suund of
    'U' -> if validatorU tabeel blokinimi == True
      then Just(liigutamineU blokinimi tabeel [])
      else error "U"
    'D' -> if validatorD tabeel blokinimi == True
      then Just(tabeel) 
      else error "D"
    'R' -> if validatorR tabeel blokinimi == True
      then Just(tabeel)
      else error "R"
    'L' -> if validatorL tabeel blokinimi == True
      then Just(tabeel)
      else error "L"
    '_' -> error "Vale suund - suundadeks on U, D, R ja L"



validatorU :: Table -> Int -> Bool

validatorU (T (x,y) (x2, y2) []) nimi = error "Selline plokk puudub"
validatorU (T (x,y) (x2, y2) ((f,((a,b):hs)):ws)) nimi = 
  if f == nimi
    then kasPunktVaba (x,y) ((f,((a,b):hs)):ws) (rekurU ((a,b):hs) (a,999)) 
    else validatorU (T (x,y) (x2, y2) ws) nimi

rekurU :: [Point] -> Point -> Point
rekurU [] (x,y) = (x,y-1) 
rekurU ((a,b):hs) (x,y) =  
  if x /= a
    then error "Plokk on horistontaalis või mitte sirge - ei saa liigutada üles"
    else if b < y
      then rekurU hs (a,b)
      else rekurU hs (x,y) 



kasPunktVaba :: Point -> [(Int,[Point])] -> Point -> Bool
kasPunktVaba (x,y) [] (r,t) = 
  if r >= x-1 || r == 0 || t == 0 || t >= x-1
    then error "Läheb välja"
    else True
kasPunktVaba (x,y) ((f,punnid):ws) (r,t) =  
  if abiRek punnid (r,t) == False
    then error "Soovitud liigutus pole võimalik"
    else kasPunktVaba (x,y) ws (r,t)
  


abiRek :: [Point] -> Point -> Bool
abiRek [] (x,y) = True
abiRek ((a,b):hs) (x,y) = 
  if x == a && y == b
    then False
    else abiRek hs (x,y)



liigutamineU :: Int -> Table -> [(Int,[Point])] -> Table
liigutamineU nimi (T (x,y) (x2, y2) ((f,punnid):ws)) irw =
  if f == nimi 
    then (T (x,y) (x2, y2) (irw ++ [(f, (asendamineU punnid))] ++ ws))
    else liigutamineU nimi (T (x,y) (x2, y2) ws) (irw ++ [(f,punnid)])


asendamineU :: [Point] -> [Point]
asendamineU [] = []
asendamineU ((a,b):ws) = [(a, b-1)] ++ asendamineU ws



validatorD :: Table -> Int -> Bool
validatorD (T (x,y) (x2, y2) (w:ws)) nimi = True

validatorR :: Table -> Int -> Bool
validatorR (T (x,y) (x2, y2) (w:ws)) nimi = True

validatorL :: Table -> Int -> Bool
validatorL (T (x,y) (x2, y2) (w:ws)) nimi = True


main = do  
  contents <- readFile "laud.txt"
  let Just(tabel) = readT contents
  putStrLn (showT tabel)
  putStrLn ((prindiTabel tabel))
  let Just(minutabel) = move (2, 'U') tabel
  putStrLn (prindiTabel minutabel)
  putStrLn (showT minutabel)







 
