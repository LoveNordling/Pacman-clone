module Core.Board.Board (Board, Tiles, createBoard) where
import Data.Array
import qualified Core.Board.Tile as Tile
import Debug.Trace

type Board = Array (Int, Int) Tile.Tile

-- The elements of a board
type Tiles = [Tile.Tile]

type Matrix = [Tiles]

{- createBoard m
   PRE:       Each row in m must have the same number of elements.
   POST:      An indexable list with elements from m, where an elements index is the position of the element in m.
   EXAMPLES:  generateBoard [ [baseWall, baseWall], [baseFloor, baseWall] ] == array ((0,0),(1,1)) [((0,0), Wall (0,0)), ((0,1), Wall (0,1)), ((1,0), Floor (1,0)), ((1,1), Wall (1,1))]
-}
createBoard :: Matrix -> Board
createBoard board =
  let
    rows = length (board) - 1
    cols = length (board !! 0) - 1
  in
    listArray ( (0,0), (cols, rows) ) (generateBoard board 0 [])
    where
      {- generateBoard m r acc
         PRE:       Each row in m must have the same number of elements.
         POST:      acc with tiles based on elements in m.
         EXAMPLES:  generateBoard [ [baseWall, baseWall], [baseFloor, baseWall] ] 0 [] == [Wall (0,0), Wall (0,1), Floor (1,0), Wall (1,1)]
         VARIANT:   |m|
      -}
      generateBoard :: Matrix -> Int -> Tiles -> Tiles
      generateBoard []     _ acc = acc
      generateBoard matrix x acc =
        let
          (column, matrix') = getFirstColumn matrix
        in
          generateBoard matrix' (x+1) (acc ++ generateColumn column 0)
        where
          {- generateColumn b c
             PRE:       True
             POST:      Tiles based on b with position c.
             EXAMPLES:  generateRow [ baseWall, baseWall ] 0 0 == [ Wall (0,0), Wall (0,1) ]
                        generateRow [ baseWall, baseWall ] 1 0 == [ Wall (1,0), Wall (1,1) ]
                        generateRow [ ] 0 0                    == [ ]
             VARIANT:   |b|
          -}
          generateColumn :: Tiles -> Int -> Tiles
          generateColumn [] _ = []
          generateColumn ts y =
            let
              t = last ts
              ts' = init ts
            in
              (Tile.setPosition t x y):(generateColumn ts' (y+1))
    		  {- getFirstColumn m
    		     PRE: TRUE
    			 POST: The first column of m and matrix that is left when removing that column
    			 EXAMPLES:
    			 VARIANT: height of m
    		  -}
          getFirstColumn :: Matrix -> (Tiles, Matrix)
          getFirstColumn []          = ([],[])
          getFirstColumn ([t]:ts)    = (t:(fst (getFirstColumn ts)), [])
          getFirstColumn ((x:xs):ys) =
            let
              column = x : (fst (getFirstColumn ys))
              matrix = xs : (snd (getFirstColumn ys))
            in
              (column, matrix)
