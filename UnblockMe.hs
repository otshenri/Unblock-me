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
getHole reanumber [] = (0,0)
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


----------------------------------------------------------------------------------

--for debugging
prindiTabel:: Table -> String
prindiTabel (T (x,y) (x2, y2) blokid) =
  show(x) ++ show(y) ++ show(x2) ++ show(y2) ++ show(blokid)



type Move  = (Int, Char)
move :: Move -> Table -> Maybe Table
move (blokinimi, suund) tabeel@(T (x,y) (x2, y2) koik@(w:ws)) = 
  case suund of
    'U' -> if validatorU tabeel koik blokinimi == True
      then Just(liigutamineU blokinimi tabeel [])
      else error "Ei saa üles liigutada"
    'D' -> if validatorD tabeel koik blokinimi == True
      then Just(liigutamineD blokinimi tabeel []) 
      else error "Ei saa alla liigutada"
    'R' -> if validatorR tabeel koik blokinimi == True
      then Just(liigutamineR blokinimi tabeel [])
      else error "Ei saa paremale liigutada"
    'L' -> if validatorL tabeel koik blokinimi == True
      then Just(liigutamineL blokinimi tabeel [])
      else error "Ei saa vasakule liigutada"
    _ -> error "Vale suund - suundadeks on U, D, R ja L"

--------------------------------------------------------------------

validatorU :: Table -> [(Int,[Point])] -> Int -> Bool
validatorU (T (x,y) (x2, y2) []) _ nimi = error "Selline plokk puudub"
validatorU (T (x,y) (x2, y2) r@((f,((a,b):hs)):ws)) listike nimi = 
  if f == nimi
    then kasPunktVaba (x,y) listike (rekurU ((a,b):hs) (a,999))  
    else validatorU (T (x,y) (x2, y2) ws) listike nimi 



validatorD :: Table -> [(Int,[Point])] -> Int -> Bool
validatorD (T (x,y) (x2, y2) []) _ nimi = error "Selline plokk puudub"
validatorD (T (x,y) (x2, y2) r@((f,((a,b):hs)):ws)) listike nimi = 
  if f == nimi
    then kasPunktVaba (x,y) listike (rekurD ((a,b):hs) (a,0)) 
    else validatorD (T (x,y) (x2, y2) ws) listike nimi 



validatorL :: Table -> [(Int,[Point])] -> Int -> Bool
validatorL (T (x,y) (x2, y2) []) _ nimi = error "Selline plokk puudub"
validatorL (T (x,y) (x2, y2) r@((f,((a,b):hs)):ws)) listike nimi = 
  if f == nimi
    then kasPunktVaba (x,y) listike (rekurL ((a,b):hs) (999, b)) 
    else validatorL (T (x,y) (x2, y2) ws) listike nimi 



validatorR :: Table -> [(Int,[Point])] -> Int -> Bool
validatorR (T (x,y) (x2, y2) []) _ nimi = error "Selline plokk puudub"
validatorR (T (x,y) (x2, y2) r@((f,((a,b):hs)):ws)) listike nimi = 
  if f == nimi
    then kasPunktVaba (x,y) listike (rekurR ((a,b):hs) (0, b)) 
    else validatorR (T (x,y) (x2, y2) ws) listike nimi 


-----------------------------------------------------------------------------------

rekurU :: [Point] -> Point -> Point
rekurU [] (x,y) = (x,y-1) 
rekurU ((a,b):hs) (x,y) =  
  if x /= a
    then ((-1), (-1))--error "Plokk on horistontaalis või mitte sirge - ei saa liigutada üles"
    else if b < y
      then rekurU hs (a,b)
      else rekurU hs (x,y) 


rekurD :: [Point] -> Point -> Point
rekurD [] (x,y) = (x,y+1) 
rekurD ((a,b):hs) (x,y) =  
  if x /= a
    then ((-1), (-1))--error "Plokk on horistontaalis või mitte sirge - ei saa liigutada alla"
    else if b > y
      then rekurD hs (a,b)
      else rekurD hs (x,y)


rekurL :: [Point] -> Point -> Point
rekurL [] (x,y) = (x-1,y) 
rekurL ((a,b):hs) (x,y) =  
  if y /= b
    then ((-1), (-1))--error "Plokk on vertikaalis või mitte sirge - ei saa liigutada paremale"
    else if a < x
      then rekurL hs (a,b)
      else rekurL hs (x,y)


rekurR :: [Point] -> Point -> Point
rekurR [] (x,y) = (x+1,y) 
rekurR ((a,b):hs) (x,y) =  
  if y /= b
    then ((-1), (-1))--error "Plokk on vertikaalis või mitte sirge - ei saa liigutada vasakule"
    else if a > x
      then rekurR hs (a,b)
      else rekurR hs (x,y)

------------------------------------------------------------------------

kasPunktVaba :: Point -> [(Int,[Point])] -> Point -> Bool
kasPunktVaba (x,y) [] (r,t) = 
  if r == x || r == 0 || t == 0 || t == y || r == (-1)
    then False
    else True
kasPunktVaba (x,y) ((f,punnid):ws) (r,t) =  
  if abiRek punnid (r,t) == False
    then False
    else kasPunktVaba (x,y) ws (r,t)
  


abiRek :: [Point] -> Point -> Bool
abiRek [] (x,y) = True
abiRek ((a,b):hs) (x,y) = 
  if x == a && y == b
    then False
    else abiRek hs (x,y)

--------------------------------------------------------------------------------

liigutamineU :: Int -> Table -> [(Int,[Point])] -> Table
liigutamineU nimi (T (x,y) (x2, y2) ((f,punnid):ws)) irw =
  if f == nimi 
    then (T (x,y) (x2, y2) (irw ++ [(f, (asendamineU punnid))] ++ ws))
    else liigutamineU nimi (T (x,y) (x2, y2) ws) (irw ++ [(f,punnid)])


asendamineU :: [Point] -> [Point]
asendamineU [] = []
asendamineU ((a,b):ws) = [(a, b-1)] ++ asendamineU ws

---------------------------------------------------------------------

liigutamineD :: Int -> Table -> [(Int,[Point])] -> Table
liigutamineD nimi (T (x,y) (x2, y2) ((f,punnid):ws)) irw =
  if f == nimi 
    then (T (x,y) (x2, y2) (irw ++ [(f, (asendamineD punnid))] ++ ws))
    else liigutamineD nimi (T (x,y) (x2, y2) ws) (irw ++ [(f,punnid)])


asendamineD :: [Point] -> [Point]
asendamineD [] = []
asendamineD ((a,b):ws) = [(a, b+1)] ++ asendamineD ws


---------------------------------------------------------------------------

liigutamineL :: Int -> Table -> [(Int,[Point])] -> Table
liigutamineL nimi (T (x,y) (x2, y2) ((f,punnid):ws)) irw =
  if f == nimi 
    then (T (x,y) (x2, y2) (irw ++ [(f, (asendamineL punnid))] ++ ws))
    else liigutamineL nimi (T (x,y) (x2, y2) ws) (irw ++ [(f,punnid)])


asendamineL :: [Point] -> [Point]
asendamineL [] = []
asendamineL ((a,b):ws) = [(a-1, b)] ++ asendamineL ws


---------------------------------------------------------------------------


liigutamineR :: Int -> Table -> [(Int,[Point])] -> Table
liigutamineR nimi (T (x,y) (x2, y2) ((f,punnid):ws)) irw =
  if f == nimi 
    then (T (x,y) (x2, y2) (irw ++ [(f, (asendamineR punnid))] ++ ws))
    else liigutamineR nimi (T (x,y) (x2, y2) ws) (irw ++ [(f,punnid)])


asendamineR :: [Point] -> [Point]
asendamineR [] = []
asendamineR ((a,b):ws) = [(a+1, b)] ++ asendamineR ws


---------------------------------------------------------------------------




testik:: Table -> Point -> Bool
testik (T (x,y) (x2, y2) blokid) (t,t1) = 
  kasPunktVaba (x,y) blokid (t,t1)
  


mangimine tabel = do
  putStrLn (showT tabel)
  let validmoved = validMoves tabel
  print validmoved
  putStrLn("Sisesta bloki number, mida soovid liigutada: ")
  s <- getLine
  let d = (read s :: Int)
  putStrLn("Sisesta suuna täht - kas U(üles), D(alla), L(vasakule), R(paremale): ")
  f <- getLine
  let g = head f

  
  if (d, g) `elem` validmoved
    then let Just(tabel2) = move(d, g) tabel
         in if winCond tabel2 == True
           then putStrLn("Võitsid")
           else mangimine tabel2
    else mangimine tabel

{-}
  let Just(tabel2) = move(d, g) tabel
  if winCond tabel2 == True
    then putStrLn("Võitsid")
    else mangimine tabel2
  -}

main = do
  contents <- readFile "laud.txt"
  let Just(tabel) = readT contents
  mangimine tabel


-------------------------------------------------------------------  

 --Kas andmestruktuur on õige ja võidutingimus täidetud
isValidTable :: Table -> Bool
isValidTable (T (x,y) (x2, y2) blokid) =
  if x2 == 0 || y2 == 0 || x<2 || y<2
    then False
    else checkTuples (x,y) blokid
  
checkTuples:: Point -> [(Int,[Point])] -> Bool
checkTuples _ [] = True
checkTuples (x,y) (t:ts) =
  let yhePoindiList = snd t
  in if checkYhePoindiListi (x,y) yhePoindiList
       then checkTuples (x,y) ts
       else False
  
checkYhePoindiListi:: Point -> [Point] -> Bool
checkYhePoindiListi _ [] = True
checkYhePoindiListi (x,y) ((x2,y2):ts) =
 if x <= x2 || y <= y2
   then False
   else checkYhePoindiListi (x,y) ts

winCond:: Table -> Bool
winCond (T (x,y) (x2, y2) (t:ts)) = 
  let nullPunktid = snd t
  in winCondAbi (x2, y2) nullPunktid
  
winCondAbi:: Point -> [Point] -> Bool
winCondAbi _ [] = False
winCondAbi (x,y) ((x2,y2):ts) =
  if (x-1)==x2 && y==y2
    then True
    else winCondAbi (x,y) ts

validMoves:: Table -> [Move]
validMoves tabel@(T (x,y) (x2, y2) blokid) =
  let blokkideNumbrid = getBlockNumbers blokid []
    in getValidMoves [] tabel blokkideNumbrid

getValidMoves:: [Move] -> Table -> [Int] -> [Move]
getValidMoves validmoved _ [] = validmoved
getValidMoves validmoved tabel@(T (t,y) (x2, y2) blokid) (x:xs) =
  if validatorU tabel blokid x
    then getValidMoves ((x, 'U') : validmoved) tabel xs
    else if validatorD tabel blokid x
      then getValidMoves ((x, 'D') : validmoved) tabel xs
      else if validatorR tabel blokid x
        then getValidMoves ((x, 'R') : validmoved) tabel xs
        else if validatorL tabel blokid x
          then getValidMoves ((x, 'L') : validmoved) tabel xs
          else getValidMoves validmoved tabel xs
  
getBlockNumbers:: [(Int,[Point])]-> [Int] -> [Int]
getBlockNumbers [] listike = listike
getBlockNumbers ((x,y):xs) listike =
  if x `elem` listike
    then getBlockNumbers xs listike
    else getBlockNumbers xs (x : listike)