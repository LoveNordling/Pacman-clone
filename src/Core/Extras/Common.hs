module Core.Extras.Common
        ( Position, setCoordinate, zero )
where

import Graphics.Gloss

--  Position class is used for Int and Float
--  TODO: Write a description here...
--
class (Num a) => Position a where
  {- setCoordinate c d p
     PRE:       True
     POST:      p with new coordinates based on c and d.
     EXAMPLES:  setCoordinate  ==
  -}
  setCoordinate :: (a, a) -> Int -> Picture -> Picture
  {- zero p
     PRE:       True
     POST:      True if p is (0, 0), otherwise False.
     EXAMPLES:  zero (0,0) == True
  -}
  zero :: (a, a) -> Bool

instance Position Int where
  zero p = p == (0,0)
  setCoordinate (x, y) d p =
    let
      newX = fromIntegral (x * d) - (fromIntegral d/2) - (10* fromIntegral d) / 2
      newY = fromIntegral (y * d) - (fromIntegral d/2) - (10* fromIntegral d) / 2
    in
      translate newX newY p

instance Position Float where
  zero p = p == (0,0)
  setCoordinate (x, y) d p =
    let
      newX =  (x * (fromIntegral d)) - ( fromIntegral d / 2) - (10* fromIntegral d) / 2
      newY =  (y * (fromIntegral d)) - ( fromIntegral d / 2) - (10* fromIntegral d) / 2
    in
      translate newX newY p

-- Apparently Gloss already provides this with its Point type.
-- instance (Num a, Num b) => Num (a, b) where
--   (a, b) + (c, d) = (a+c, b+d)
--   (a, b) - (c, d) = (a-c, b-d)
--   (a, b) * (c, d) = (a*c, b*d)
--   abs (a, b)      = (abs a, abs b)
--   signum (a, b)   = (signum a, signum b)
--   fromInteger a   = (fromInteger a, fromInteger a)
