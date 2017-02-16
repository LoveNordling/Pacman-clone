import Tile
import Test.HUnit
import Debug.Trace
import Data.Maybe
import PriorityQueue

type Path' = [Coords]
type Coords = (Float, Float)

astar :: [Tile] -> Coords -> Tile -> PriorityQueue Tile -> [Tile] -> Path'
astar board goal current open closed
  | goal == tileCoords current = [goal]
  | otherwise                  = let
                                   closed' = current:closed
                                   ns = filterTiles (neighbours current board) closed -- läs in alla grannar och filtrera bort redan besökta
                                   i = map (distanceToGoal goal) ns                   -- ta reda på deras avstånd till målet
                                   open' = foldl addToOpenList open (zip i ns)        -- lägg till i open
                                 in
                                   astar board goal (fst $ least open') open' closed' -- upprepa, måste komma på hur jag branchar denna. kanske med en stack som kan backtracka om man når basfallet utan att ha hittat målet. Fan, måste få in travel cost här också. Iofs bara att räkna uppåt eftersom avståndet redan är där. 
                                      

{- neighbours current board
   POST: [All neigbouring tiles to current]
-}                            


neighbours :: Tile-> [Tile] -> [Tile]
neighbours (Tile (x,y) (t, l, b ,r) _) board = 
  let
    left   = neighbour l (x-1, y  ) board
    right  = neighbour r (x+1, y  ) board
    top    = neighbour t (x  , y-1) board
    bottom = neighbour b (x  , y+1) board
    
    neighbour :: Bool -> Coords -> [Tile] -> Maybe Tile
    neighbour _ _  []  = Nothing                             -- basfall, tom plan
    neighbour True _ _ = Nothing                             -- är boolen true är det en vägg ivägen
    neighbour b (x, y) ((Tile (x',y') bools act):xs) 
      | x==x' && y==y' = Just (Tile (x',y') bools act)       -- koordinaten vi söker
      | otherwise      = neighbour b (x, y) xs               -- annars rekuresera
  in
    map fromJust $ filter isJust [left, right, top, bottom]  -- filtrerar bort Nothings och lyfter ur alla "Just v" till "v"



{- filterTiles a b
   POST: all elements in a that are not in b
-}
filterTiles :: [Tile] -> [Tile] -> [Tile]
filterTiles tiles filterList = filter (`notElem` filterList) tiles

{- distancetoGoal (x,y) tile
   POST: number of moves required to reach t
-}
distanceToGoal :: Coords -> Tile -> Float
distanceToGoal (x,y) tile = let (x',y') = tileCoords tile in (abs (x'-x)) + (abs(y'-y))


tileCoords :: Tile -> Coords
tileCoords (Tile (x,y) _ _) = (x,y)

{- pop stack
   POST: (head of stack, rest of stack)
-}
pop :: [a] -> (a,[a])
pop [] = error "Empty stack"
pop [t] = (t, [])
pop ts = (head ts, tail ts)

{- push x stack
   POST: x prepended to stack
 -}
push :: a -> [a] -> [a]
push a as = a:as

{- removeFromOpenList t openlist
   POST: openlist with t removed
-}
removeFromOpenList :: (Float, Tile) -> PriorityQueue Tile -> PriorityQueue Tile
removeFromOpenList t pq
  | PriorityQueue.exists pq t = PriorityQueue.remove pq t
  | otherwise                 = pq

addToOpenList ::    PriorityQueue Tile -> (Float, Tile) -> PriorityQueue Tile
addToOpenList pq t = PriorityQueue.insert pq t

test1 = let
         current = Tile (0.0,0.0) (True, True, True,False) Void
         board =   [Tile (0.0,0.0) (True,True,True, False) Void,
                    Tile (1.0,0.0) (True, False, True, False) Void,
                    Tile (2.0,0.0) (True, False, True, True) Void]
         in
           TestCase $ assertEqual "neighbours"
           ([Tile (1.0,0.0) (True, False, True, False) Void]) (neighbours current board)
