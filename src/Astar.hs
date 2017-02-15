import Tile
import Test.HUnit
import Data.Maybe
type Coords = (Int, Int)
type TileTuple = (Coords, Tile)

{- neighbours (x, y) ts
   POST: All neighbouring tiles for the tile at coordinate (x, y)
   EXAMPLES: neigbours (1,1) (Tile (True, True, True, False) Void) [(2,1), Tile (True, False, True, True) Void)] = [Just ((Tile (True, False, True, True) Void))]
-}
neighbours :: Coords -> Tile-> [TileTuple] -> [TileTuple] -> [TileTuple] -> [Tile]
neighbours (x,y) (Tile (t, l, b ,r) _) board open closed =
  let
    left   = neighbour l (x-1, y  ) board
    right  = neighbour r (x+1, y  ) board
    top    = neighbour t (x  , y-1) board
    bottom = neighbour b (x  , y+1) board
    
    neighbour :: Bool -> Coords -> [TileTuple] -> Maybe Tile
    neighbour _ _  []  = Nothing -- basfall, tom plan
    neighbour True _ _ = Nothing -- är boolen true är det en vägg ivägen
    neighbour b (x, y) (((x',y'), t):xs) 
      | x==x' && y==y' = Just t  -- koordinaten vi söker
      | otherwise      = neighbour b (x, y) xs -- annars rekuresera
      
    justList = filter isJust [left, right, top, bottom] --filtrerar bort Nothings
  in
    map fromJust justList --Lyfter ur alla "Just v" till "v"

{- filterClosedNeighbours closed ns
   POST: all elements in ns not present in closed
-}
filterClosedNeighbours :: [TileTuple] -> [TileTuple] -> [TileTuple]
filterClosedNeighbours closed ns = filter (`notElem` closed) ns

{- removeFromOpenList t openlist
   POST: openlist with t removed
-}
removeFromOpenList :: TileTuple -> [TileTuple] -> [TileTuple]
removeFromOpenList t []     = []
removeFromOpenList t (x:xs)
  | t==x                    = xs
  | otherwise               = x:removeFromOpenList t xs

{- openListPop stack
   POST: (head of stack, rest of stack)
-}
openListPop :: [TileTuple] -> (TileTuple,[TileTuple])
openListPop [] = error "Empty stack"
openListPop ts = (head ts, tail ts)

addToClosedList :: TileTuple -> [TileTuple] -> [TileTuple]
addToClosedList t ts = t:ts



                 
test1 = TestCase $ assertEqual ("neighbours")
        [(Tile (True, False, True, True) Void)]
        (neighbours (1,1) (Tile (True, True, True, False) Void) [((2,1), Tile (True, False, True, True) Void)] [] [])

test2 = TestCase $ assertEqual ("neighbours")
        []
        (neighbours (1,1) (Tile (True, True, True, True) Void) [((2,1), Tile (True, False, True, True) Void)] [] [])

