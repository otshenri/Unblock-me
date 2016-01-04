import Data.Array
import System.Random


class Eq a => Dist a where
  add     :: a -> a -> a
  le      :: a -> a -> Bool
  infty   :: a
  nul     :: a
  ofFloat :: (Real b) => b -> a


instance (Floating a, Ord a) => Dist (Maybe a) where
  (Just a) `add` (Just b) = Just (a+b)
  _        `add` _        = Nothing
 
  (Just a) `le` (Just b) = a < b
  (Just _) `le` _        = True
  _        `le` _        = False
 
  infty = Nothing
  nul = ofFloat (0 :: Double)
 
  ofFloat x = Just $ fromRational $ toRational x


type WeightedGraph a b = ([a], [(a,b,a)])

e1 :: WeightedGraph Int (Maybe Float)
e1 = ([0..7],[
  (1,ofFloat 5  ,1),
  (0,ofFloat 9  ,4),
  (0,ofFloat 8  ,7),
  (1,ofFloat 12 ,2),
  (1,ofFloat 15 ,3),
  (1,ofFloat 4  ,7),
  (2,ofFloat 3  ,3),
  (2,ofFloat 11 ,6),
  (3,ofFloat 9  ,6),
  (4,ofFloat 4  ,5),
  (4,ofFloat 20 ,6),
  (4,ofFloat 5  ,7),
  (5,ofFloat 1  ,2),
  (7,ofFloat 6  ,5),
  (7,ofFloat 7  ,2)])

e2 :: WeightedGraph Int (Maybe Float)
e2 = ([], [(1, ofFloat 2, 5)])



--bellmanFord1 :: (Dist b, Ix a) => WeightedGraph a b -> a -> Array a (b,Maybe a)

bellmanFord :: (Dist b, Ix a, Num a) => WeightedGraph a b -> a -> [(a,a)]
bellmanFord ([w], []) tipp = error "Vigane graaf"
bellmanFord ([], [c]) tipp = error "Vigane graaf"
bellmanFord ([], []) tipp = error "Vigane graaf"

bellmanFord (w : ws, y : ys) tipp = 
	let esialgsedkaugused = abi (w:ws) tipp 0 9999
	in vajalik (y:ys) esialgsedkaugused tipp



abi :: Ix a => [a] -> a -> a -> a -> [(a,a)]
abi [] tipp asd psd = []
abi (x:xs) tipp asd psd = 
	if x == tipp
		then abi xs tipp asd psd ++ [(x, asd)] --siia 0 või nul "tipp" asemel
		else abi xs tipp asd psd ++ [(x, psd)] --siia lõpmatus/99999 või infty "tipp" asemel


 
vajalik :: (Dist b, Ix a) => [(a,b,a)] -> [(a,a)] -> a -> [(a, a)]
vajalik [] listike algustipp = listike ++ []
vajalik ((x,y,z):xs) listike algustipp = 
	let kaugus = rekur listike z
	in if x == algustipp && x < kaugus -- siin probleem peaks olema y < kaugus
		then listike ++ [(algustipp, kaugus)] ++ vajalik xs listike algustipp
		else vajalik xs listike algustipp




rekur :: Eq a => [(a, a)] -> a -> a
rekur [] tipp = tipp
rekur (x:xs) tipp = 
	if fst x == tipp
		then snd x
		else rekur xs tipp
	




{-
lel :: (Dist b, Ix a) => WeightedGraph a b -> a -> Array a b
lel ([w], []) tipp = error "Vigane graaf"
lel ([], [c]) tipp = error "Vigane graaf"
lel ([], []) tipp = error "Vigane graaf"

lel (w : ws, (q,e,r) : ys) tipp = array (w,x) [(r,e)]



rndWG :: Int -> IO (WeightedGraph Int (Maybe Float))
rndWG mitu = do
	x <- ([0..mitu], [(1, ofFloat 2, 5)])
	return x 


lisaKaari :: (Dist b, Ix a) => Int -> (a,b,a)
lisaKaari 0 = Nothing
lisaKaari number = (1, ofFloat 2, 1)
-}

