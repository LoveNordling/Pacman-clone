import Tile
import Test.HUnit
import Data.Maybe
type Coords = (Int, Int)
type TileTuple = (Coords, Tile)
type Path' = [Coords]

testMap :: [TileTuple]
testMap = [((0,0), Tile (True, True, False, False) Void), ((0,1), Tile (True, False, True, True) Void), ((1,0), Tile (False, True, True, True) Void)]

testTile :: TileTuple
testTile = ((0,0), Tile (True, True, False, False) Void)

findGoal :: TileTuple -> TileTuple  -> [TileTuple] ->  [TileTuple] -> [(Int, TileTuple)]-> Maybe Path'
findGoal (currentPos, currentTile) (goalPos, goalTile) board closed open
  | currentPos == goalPos = Just [(0,0)]
  | otherwise             = undefined

{- neighbours (x, y) ts
   POST: All neighbouring tiles for the tile at coordinate (x, y)
   EXAMPLES: neigbours (1,1) (Tile (True, True, True, False) Void) [(2,1), Tile (True, False, True, True) Void)] = [Just ((Tile (True, False, True, True) Void))]
-}                            
neighbours :: Coords -> Tile -> [TileTuple] -> Coords-> [(Int, TileTuple)]
neighbours (x,y) t board goalCoords =
  let
    ns = neighboursAux (x, y) t board
    dist = map (distanceToGoal goalCoords) ns
  in
    zip dist ns 

neighboursAux :: Coords -> Tile-> [TileTuple] -> [TileTuple]
neighboursAux (x,y) (Tile (t, l, b ,r) _) board =
  let
    left   = neighbour l (x-1, y  ) board
    right  = neighbour r (x+1, y  ) board
    top    = neighbour t (x  , y-1) board
    bottom = neighbour b (x  , y+1) board
    
    neighbour :: Bool -> Coords -> [TileTuple] -> Maybe TileTuple
    neighbour _ _  []  = Nothing -- basfall, tom plan
    neighbour True _ _ = Nothing -- är boolen true är det en vägg ivägen
    neighbour b (x, y) (((x',y'), t):xs) 
      | x==x' && y==y' = Just ((x, y),t)  -- koordinaten vi söker
      | otherwise      = neighbour b (x, y) xs -- annars rekuresera
  in
    map fromJust $ filter isJust [left, right, top, bottom]  --filtrerar bort Nothings och lyfter ur alla "Just v" till "v"



    
distanceToGoal :: Coords -> TileTuple -> Int
distanceToGoal (x,y) ((x',y'),_) = (abs (x'-x)) + (abs(y'-y))

{- filterClosedNeighbours closed ns
   POST: all elements in ns not present in closed
-}
filterClosedNeighbours :: [TileTuple] -> [TileTuple] -> [TileTuple]
filterClosedNeighbours closed ns = filter (`notElem` closed) ns


{- openListPop stack
   POST: (head of stack, rest of stack)
-}
openListPop :: [TileTuple] -> (TileTuple,[TileTuple])
openListPop [] = error "Empty stack"
openListPop [t] = (t, [])
openListPop ts = (head ts, tail ts)

addToClosedList :: TileTuple -> [TileTuple] -> [TileTuple]
addToClosedList t ts = t:ts

{- removeFromOpenList t openlist
   POST: openlist with t removed
-}
removeFromOpenList :: TileTuple -> [TileTuple] -> [TileTuple]
removeFromOpenList t []     = []
removeFromOpenList t (x:xs)
  | t==x                    = xs
  | otherwise               = x:removeFromOpenList t xs

addToOpenList ::  [(Int, TileTuple)] -> (Int, TileTuple) -> [(Int, TileTuple)]
addToOpenList [] (n, t) = [(n,t)]
addToOpenList ((n',t'):xs) (n, t)
  | n<n' = (n,t):(n',t'):xs
  | otherwise = (n',t'):addToOpenList xs (n,t)
        




