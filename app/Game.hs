{-# LANGUAGE Arrows, BangPatterns #-}

module Game where

import FRP.Yampa
import FRP.Yampa.Vector2

type Vector = Vector2 Float
type Size = Vector

data Ball = Ball { ballRadius :: Float, ballPos :: Vector, ballVel :: Vector }

x = vector2X :: Vector -> Float
y = vector2Y :: Vector -> Float
gravity = vector2 0 (-9.8 * 50) :: Vector

ball :: Float -> Vector -> Vector -> Ball
ball radius position velocity = Ball { ballRadius = radius, ballPos = position, ballVel = velocity }

createBall :: Ball -> SF a Ball
createBall ball = proc _ -> do
    vel <- (^+^ ballVel ball) ^<< integral -< gravity
    pos <- (^+^ ballPos ball) ^<< integral -< vel
    returnA -< Ball { ballRadius = ballRadius ball, ballPos = pos, ballVel = vel }

manyBalls :: Size -> Ball -> SF a [Ball]
manyBalls s b = parB . take 10 . repeat $ ballInBox s b

ballInBox :: Size -> Ball -> SF a Ball
ballInBox size ball = 
    let sfBall = createBall ball
        bounceEvent = (\b -> checkBounce size b) ^<< sfBall
    in switch (sfBall &&& bounceEvent) (\b -> ballInBox size b)

checkBounce :: Size -> Ball -> Event Ball
checkBounce s b =
    let r = ballRadius b
        p = ballPos b
        v = ballVel b
    in if x p - r < 0   then Event $ ball r (vector2 (0 + r) (y p)) (vector2 (-(x v)) (y v)) else
       if x p + r > x s then Event $ ball r (vector2 (x s - r) (y p)) (vector2 (-(x v)) (y v)) else
       if y p - r < 0   then Event $ ball r (vector2 (x p) (0 + r)) (vector2 (x v) (-(y v))) else
       if y p + r > y s then Event $ ball r (vector2 (x p) (y s - r)) (vector2 (x v) (-(y v))) else NoEvent