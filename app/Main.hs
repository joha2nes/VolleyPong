{-# LANGUAGE Arrows #-}

import FRP.Yampa
import FRP.Yampa.Vector2
import Graphics.Gloss.Interface.FRP.Yampa
import Graphics.Gloss
import Game
import VolleyPongGloss
import qualified Graphics.Gloss.Interface.Pure.Game as G

main :: IO ()
main =
    let width = 1000
        height = 600
        hertz = 120
    in playYampa (InWindow "Hej" (width, height) (0, 0))
                 black
                 hertz
                 appl