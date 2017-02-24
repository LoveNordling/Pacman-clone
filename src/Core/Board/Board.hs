module Core.Board.Board
        ( Board, Tiles, createBoard, testSuite ) where

import Test.HUnit
import Data.Array
import qualified Core.Board.Tile as Tile

-- A board is just an array with Int tuples as keys and a Tile as its elements
-- The keys represents the coordinate of the tiles.
type Board = Array (Int, Int) Tile.Tile

-- The elements of a board
type Tiles = [Tile.Tile]

-- A matrix of tiles
type Matrix = [Tiles]
baseWall = Tile.Wall (0,0)
baseFloor = Tile.Floor (0,0) False
{- createBoard m
   PRE:           Each row in m must have the same number of elements
   POST:          An indexable list with elements from m, where an element's index is the position of the element in m
   SIDE EFFECTS:  None
   EXAMPLES:      createBoard [ [baseWall, baseWall], [baseFloor, baseWall] ] gives an array with the first element Wall (0,0) at index (0,0), the second element Wall (0,1) at index (0,1), the third element Floor (1,0) at index (1,0) and fourth element Wall (1,1) at index (1,1)
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
         PRE:           Each row in m must have the same number of elements
         POST:          acc with tiles based on elements in m
         SIDE EFFECTS:  None
         EXAMPLES:      generateBoard [ [baseWall, baseWall], [baseFloor, baseWall] ] 0 [] == [Wall (0,0), Wall (0,1), Floor (1,0), Wall (1,1)]
         VARIANT:       |m|
      -}
      generateBoard :: Matrix -> Int -> Tiles -> Tiles
      generateBoard []     _ acc = acc
      generateBoard matrix x acc = generateBoard matrix' (x+1) (acc ++ generateColumn column 0)
        where
          -- First column of the matrix, and the rest of the matrix
          (column, matrix') = getFirstColumn matrix
          {-  generateColumn b c
              PRE:            True
              POST:           Tiles based on b with y position c.
              SIDE EFFECTS:   None
              EXAMPLES:       generateColumn [ Wall (0,0), Wall (0,0) ] 0 == [ Wall (0,0), Wall (0,1) ]
              VARIANT:        |b|
          -}
          generateColumn :: Tiles -> Int -> Tiles
          generateColumn [] _ = []
          generateColumn ts y = (Tile.setPosition (last ts) x y):(generateColumn (init ts) (y+1))
          {-  getFirstColumn m
              PRE:          True
              POST:         The first column of m and the matrix that is left when removing that column
              SIDE EFFECTS: None
              EXAMPLES:     getFirstColumn [ [ Wall (0,0), Wall (0,0) ], [ Floor (0,0) False,  Floor (0,0) False ] ] gives a tuple with first component a list consisting of a Wall (0,0) and a Floor (0, 0) and the rest of the inputed matrix [ [ Wall (0,0), Floor (0,0) False ] ]
              VARIANT:      height of m
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

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1 :: Test
testSuite = TestList [ test1 ]
-- createBoard
test1 =
  let
    baseWall  = Tile.Wall (0,0)
    baseFloor = Tile.Floor (0,0) False
    board     = [ [baseWall, baseWall, baseWall], [baseWall, baseFloor, baseWall], [baseWall, baseWall, baseWall] ]
  in
    TestLabel "Generate Board Test #1" .
      TestCase $ assertEqual "" (Tile.Wall (2,2)) ((createBoard board) ! (2, 2))
