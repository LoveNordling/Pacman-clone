module Main where
import Graphic
import Board

tiles = [
  Tile (True, True, True, False) Void,
  Tile (True, True, True, False) Void,
  Tile (False, False, False, True) Void,
  Tile (True, False, False, True) Void
  ]

main :: IO ()
main = Graphic.generateWindow tiles
