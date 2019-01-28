{-# LANGUAGE Arrows #-}

module VolleyPongGloss where

import VolleyPong

import Control.Monad
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Debug.Trace
import FRP.Yampa hiding (fromEvent)
import FRP.Yampa.Vector2
import Graphics.Gloss
import Graphics.Gloss.Interface.FRP.Yampa
import qualified Graphics.Gloss.Interface.Pure.Game as G
import Prelude hiding (Left, Right)
import Types
import Rect

netPicture :: Picture
netPicture = rectPicture gameNet

paddlePicture :: Paddle -> Picture
paddlePicture = rectPicture . paddleRect
            
ballPicture :: Ball -> Picture
ballPicture = rectPicture . ballRect 

gamePicture :: Game -> Picture
gamePicture g = color white $ translate 0 (-250) $ pictures [paddlePicture $ gamePaddle1 g, -- paddle 1
                                                             paddlePicture $ gamePaddle2 g, -- paddle 2
                                                             ballPicture $ gameBall g, -- ball
                                                             scale 0.8 0.8 $ translate (-300) 400 $ text $ show $ fst $ gameScores g, -- score 1
                                                             scale 0.8 0.8 $ translate (300) 400 $ text $ show $ snd $ gameScores g, -- score 2
                                                             gameFieldPicture,
                                                             netPicture]

gameFieldPicture :: Picture
gameFieldPicture = line [leftCorner, rightCorner]
    where
        leftCorner = (realToFrac $ -fieldWidth / 2, 0)
        rightCorner = (realToFrac $ fieldWidth / 2, 0)

rectPicture :: Rect -> Picture
rectPicture (Rect position size) = translate (x' + w' / 2) (y' + h' / 2) $ rectangleSolid w' h'
    where
        x' = realToFrac $ x position
        y' = realToFrac $ y position
        w' = realToFrac $ x size
        h' = realToFrac $ y size

keyDown :: Char -> SF (Event InputEvent) Bool
keyDown c =
    let f :: Char -> InputEvent -> Event Bool
        f c (G.EventKey (G.Char c') s _ _) = if c == c' then Event (s == G.Down) else NoEvent
        f c _ = NoEvent
    in hold False <<< arr (join . fmap (f c))

inputDirection :: (Char, Char) -> SF (Event InputEvent) (Maybe Direction)
inputDirection (l, r) =
    let inputs = [boolSwitch Left ^<< keyDown l,
                    boolSwitch Right ^<< keyDown r]
    in join . find isJust ^<< parB inputs

inputRestart :: SF (Event InputEvent) Bool
inputRestart = keyDown 'r'

inputPause :: SF (Event InputEvent) Bool
inputPause = keyDown 'p'

appl :: SF (Event InputEvent) Picture
appl = gamePicture ^<< pause game0 inputPause (game <<< input)
    where
        game0 = undefined
        input :: SF (Event InputEvent) GameInput
        input = proc i -> do
            d1 <- inputDirection ('a', 'd') -< i
            d2 <- inputDirection ('j', 'l') -< i
            r <- inputRestart -< i
            returnA -< GameInput r (d1, d2)
    