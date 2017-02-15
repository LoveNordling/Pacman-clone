import Tile

{- neighbours (x, y) ts
   POST: All neighbouring tiles for the tile at coordinate (x, y)
-}
neighbours :: (Int, Int) -> Tile-> [((Int, Int), Tile)] -> [Maybe Tile]
neighbours (x,y) (Tile (t, l, b ,r) _) ts  =
  let
    left   = findNeighbour (x-1, y) ts
    right  = findNeighbour (x+1, y) ts
    top    = findNeighbour (x  , y-1) ts
    bottom = findNeighbour (x  , y+1) ts
  in
    [left, right, top, bottom]

findNeighbour :: (Int, Int) -> [((Int, Int), Tile)] -> Maybe Tile
findNeighbour _  [] = Nothing
findNeighbour (x, y) (((x',y'), t):xs)
  | x==x' && y==y' = Just t
  | otherwise      = findNeighbour (x, y) xs
