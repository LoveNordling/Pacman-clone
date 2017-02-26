module Core.Board.Level
        ( Level, getBoard, getLevelNumber, getGoal, spawnPosition, nextLevel, setLevel, checkForTreasure, testSuite, testSuiteExceptions ) where

-- Modules for testing
import Test.HUnit
import Test.Hspec
import Control.Exception (evaluate)
-- External modules
import Data.Array
-- Internal modules
import qualified Core.Board.Board as Board
import qualified Core.Board.Actor as Actor
import qualified Core.Board.Tile as Tile
import qualified Core.Extras.Resources as Resources

{-# ANN module "HLint: Ignore Use fmap" #-}

{-
  REPRESENTATION CONVENTION:
    A level is given by Level b xy n g. The first argument, b, is the board, map, of the level while the second is the default position for the AIs to spawn on. The level number is n, and g is the total number of treasures on a map that a player must collect in order to advance to the next.

  REPRESENTATION INVARIANT:
    The number of treasures on a map must be above 0. The default position of the AI must be valid positions on the board. The board must not have overlapping tiles.
-}
data Level = Level Board.Board Actor.Position Int Int
           deriving (Eq, Show) -- for testing purposes only

{- getBoard l
  PRE:           True
  POST:          The board of l
  SIDE EFFECTS:  None
  EXAMPLES:      Calling getBoard on a level gives only the board on that level.
-}
getBoard :: Level -> Board.Board
getBoard (Level b _ _ _) = b

{- getLevelNumber l
  PRE:           True
  POST:          Level number of l
  SIDE EFFECTS:  None
  EXAMPLES:      Calling getLevelNumber on a level with level number 100 gives 100
-}
getLevelNumber :: Level -> Int
getLevelNumber (Level _ _ n _) = n

{- getGoal l
  PRE:           True
  POST:          The level goal of l
  SIDE EFFECTS:  None
  EXAMPLES:      Calling getGoal on a level with goal 1 gives 1
-}
getGoal :: Level -> Int
getGoal (Level _ _ _ g) = g

{- spawnPosition l
  PRE:       True.
  POST:      Default spawn position for AIs.
  SIDE EFFECTS:
  EXAMPLES:    spawnPosition ==
-}
spawnPosition :: Level -> (Float, Float)
spawnPosition (Level _ p _ _) = p

{- nextLevel l
   PRE:           True
   POST:          A new level with level number one above the level number of l. If such a level does not exist, then Nothing
   SIDE EFFECTS:  None
   EXAMPLES:      Calling nextLevel on a level with level number 0 gives a level with number 1
-}
nextLevel :: Level -> Maybe (Level, Actor.Position)
nextLevel (Level _ _ n _) = setLevel (n+1)

{- setLevel i
   PRE:           i >= 0
   POST:          If there exists a map with number i, then a level with that map as a Board together with the starting position of the player, otherwise Nothing
   SIDE EFFECTS:  Prints an error message to the screen if i < 0.
   EXAMPLES:      setLevel 0 gives the first level
-}
setLevel :: Int -> Maybe (Level, Actor.Position)
setLevel i
  | (length Resources.levels) - 1 < i = Nothing
  | otherwise =
    let
      tiles = Resources.levels !! i
      coord = Resources.coords !! i
      board = Board.createBoard tiles
      count = countTreasures tiles
      level = Level board (snd coord) i count
    in
      Just (level, (fst coord))
    where
      {- countTreasures b
         PRE:           True
         POST:          The number of elements in b that has treasures
         SIDE EFFECTS:  None
         EXAMPLES:      Calling countTreasures on a matrix which holds only one element with a treasure gives the number 1 and nothing else
         VARIANT:       |b|
      -}
      countTreasures :: [[Tile.Tile]] -> Int
      countTreasures tiles = sum (map (foldl sumTreasures 0) tiles)
      {- findTreasures i t
         PRE:           True
         POST:          i + 1 if t has a trasure on it, otherwise i
         SIDE EFFECTS:  None
         EXAMPLES:      findTreasures 0 (Floor (0,0) False) == 0
                        findTreasures 0 (Floor (0,0) True)  == 1
         VARIANT:       |b|
      -}
      sumTreasures :: Int -> Tile.Tile -> Int
      sumTreasures i t
        | Tile.hasTreasure t = i + 1
        | otherwise = i

{- checkForTreasure l s p
   PRE:           p must be valid coordinates for the level.
   POST:          If p has treasure, then l without a treasure on that position and s + 1, otherwise l and s as they are.
   SIDE EFFECTS:  None
   EXAMPLES:      Calling checkForTreasure on a level without any treasures and the tuple (1,1) gives Nothing.
                  Calling checkForTreasure on a level with a treasure on position (1,1) and the tuple (1,1) gives the same level without a treasure on that position.
-}
checkForTreasure :: Level -> (Int, Int) -> Maybe Level
checkForTreasure level@(Level board cp n g) p =
  if foundTreasure board p
    then Just (Level (board // [(p, (Tile.Floor p False))]) cp n g)
    else Nothing
    where
      {- foundTreasure arguments
         PRE:       ...
         POST:      post-condition on the result, in terms of the arguments
         EXAMPLES:  foundTreasure ==
      -}
      foundTreasure :: Board.Board -> (Int, Int) -> Bool
      foundTreasure board position = Tile.hasTreasure (board ! position)

-------------------------------------------
-- TEST CASES
-------------------------------------------
test1, test2, test3, test4 :: Test
testSuite = TestList [ test1, test2, test3, test4 ]
-- nextLevel
test1 =
  let Just (level, _) = setLevel 0
  in  TestLabel "Next Level Test #1" .
        TestCase $ assertEqual "Should return a level with a board with 100 elements" (100) (length (getBoard level))
test2 =
  TestLabel "Next Level Test #1" .
    TestCase $ assertEqual "Should return Nothing" (Nothing) (setLevel (10))
-- checkForTreasure
test3 =
  let
    baseFloor = Tile.Floor (0,0) False
    board = Board.createBoard [[ baseFloor, baseFloor ], [ baseFloor, baseFloor], [baseFloor, baseFloor], [baseFloor, baseFloor] ]
    level = Level board ((0,0)) 0 0
  in
    TestLabel "Check For Treasure Test #1" .
      TestCase $ assertEqual "Should return Nothing" (Nothing) (checkForTreasure level (0,0))
test4 =
  let
    itemFloor = Tile.Floor (0,0) True
    baseFloor = Tile.Floor (0,0) False
    board = Board.createBoard [[ baseFloor, itemFloor ], [ baseFloor, baseFloor], [itemFloor, baseFloor], [itemFloor, baseFloor] ]
    level = Level board ((0,0)) 0 0
    check = case checkForTreasure level (0,1) of
              Nothing -> False
              _       -> True
  in
    TestLabel "Check For Treasure Test #1" .
      TestCase $ assertEqual "Should return True" (True) (check)

-- EXCEPTION TESTS
-- Using HSpec because HUnit sucks
-------------------------------------------
testSuiteExceptions = do
  describe "Set Level Exception Test #1" $ do
    it "Should throw exception: Prelude.!!: negative index" $ do
      let (Just (level,_))= setLevel (-1)
      evaluate (getBoard level) `shouldThrow` (errorCall "Prelude.!!: negative index")
