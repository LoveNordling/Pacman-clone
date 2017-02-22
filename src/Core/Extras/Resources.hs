module Core.Extras.Resources
        ( SpriteMode(..), getSprite )
where

data SpriteMode = R Bool | L Bool

{- getSprite arguments
   PRE:           pre-condition on the arguments
   POST:          post-condition on the result, in terms of the arguments
   SIDE EFFECTS:  if any, including exceptions
   EXAMPLES:      getSprite ==
   VARIANT:       None
-}
getSprite :: SpriteMode -> String
getSprite (R True) = dazzleManClosedRight
getSprite (R False) = dazzleManRight
getSprite (L True) = dazzleManClosedLeft
getSprite (L False) = dazzleManLeft

dazzleManClosedRight, dazzleManClosedLeft, dazzleManRight, dazzleManLeft :: String
dazzleManClosedRight = "res/dazzleManClosedRight.png"
dazzleManClosedLeft = "res/dazzleManClosedLeft.png"
dazzleManRight = "res/dazzleManRight.png"
dazzleManLeft = "res/dazzleManLeft.png"
