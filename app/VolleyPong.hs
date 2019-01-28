{-# LANGUAGE Arrows #-}

module VolleyPong where

import Control.Monad
import Data.Bool
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Debug.Trace
import FRP.Yampa hiding (fromEvent)
import FRP.Yampa.Vector2
import Prelude hiding (Left, Right)
import Types
import Rect

data GameInput = GameInput Bool (Maybe Direction, Maybe Direction) deriving (Eq, Ord)
data Game = Game { gamePaddle1 :: Paddle, gamePaddle2 :: Paddle, gameBall :: Ball, gameScores :: Scores }
data Paddle = Paddle { paddleRect :: Rect }
data Ball = Ball { ballRect :: Rect, ballVelocity :: Velocity } deriving (Eq, Show)
data Direction = Left | Right deriving (Eq, Ord)
type Scores = (Int, Int)
type Net = Rect

ballGravity = vector2 0 (-400)
ballBounceMagnitude = 500
netHeight = 140
fieldWidth = 800 :: Double
paddleSize = vector2 60 15

gameNet :: Net 
gameNet = let (w, h) = (10, 130) in Rect (vector2 (-w / 2) 0) (vector2 w h)

boolSwitch :: a -> Bool -> Maybe a
boolSwitch x b = if b then Just x else Nothing

paddle :: Paddle -> SF (Maybe Direction) Paddle
paddle p0 = proc mdir -> do
    dir <- arr (fromMaybe zeroVector . fmap dirVec) -< mdir
    pos <- (startPos ^+^) ^<< (speed *^) ^<< integral -< dir
    returnA -< Paddle { paddleRect = Rect pos (vector2 width height) }
    where
        startPos = rectPos $ paddleRect p0
        width = x $ rectSize $ paddleRect p0
        height = y $ rectSize $ paddleRect p0
        speed = 500
        dirVec d = case d of Left  -> vector2 (-1) 0
                             Right -> vector2 1 0

reflect :: Vector2 Double -> Vector2 Double -> Vector2 Double
reflect v n = v ^-^ 2 *^ ((dot v n) *^ n)

clampedPaddle :: (Double, Double) -> Paddle -> SF (Maybe Direction) Paddle
clampedPaddle (xMin, xMax) p0 = switch sf (clampedPaddle (xMin, xMax))
    where
        sf :: SF (Maybe Direction) (Paddle, Event Paddle)
        sf = proc mdir -> do
            p <- paddle p0 -< mdir
            e <- arr clampPaddle -< p
            returnA -< (p, e)
            where
                clampPaddle :: Paddle -> Event Paddle
                clampPaddle p' = if px < xMin || (px + x size) > xMax
                    then Event $ Paddle (Rect (vector2 (clamp xMin (xMax - x size) px) py) size)
                    else NoEvent
                    where
                        py = y $ rectPos $ paddleRect p'
                        px = x $ rectPos $ paddleRect p'
                        size = rectSize $ paddleRect p'

fallingBall :: Ball -> SF a Ball
fallingBall startBall = proc _ -> do
    vel <- (^+^ startVel) ^<< integral -< ballGravity
    pos <- (^+^ startPos) ^<< integral -< vel
    returnA -< Ball { ballRect = Rect pos size, ballVelocity = vel }
    where
        startVel = ballVelocity startBall
        (Rect startPos size) = ballRect startBall

ball :: Ball -> SF (Paddle, Paddle) Ball
ball startBall = switch sf ball
    where
        sf :: SF (Paddle, Paddle) (Ball, Event Ball)
        sf = proc (p1, p2) -> do
            b' <- fallingBall startBall -< ()
            returnA -< (b', mergeEvents [ballNetPhysics b' gameNet, checkBallPaddleColl b' p1, checkBallPaddleColl b' p2])
            where
                checkBallPaddleColl :: Ball -> Paddle -> Event Ball
                checkBallPaddleColl b p = maybeToEvent $ ballPaddleBounce b p

ballPaddleBounce :: Ball -> Paddle -> Maybe Ball
ballPaddleBounce b p = fmap newBall $ collide paddleRect' (ballRect b)
    where
        paddleRect' = paddleRect p
        newBall :: Rect -> Ball
        newBall r = Ball { ballRect = r, ballVelocity = vel }
            where
                vel = ballBounceMagnitude *^ (normalize $ vector2 vx 1)
                vx = dx / phw
                phw = pw / 2
                pw = x . size $ paddleRect'
                dx = x $ (center r) ^-^ (center paddleRect')

ballNetPhysics :: Ball -> Net -> Event Ball
ballNetPhysics ball net = maybeToEvent $ fmap onCollision $ collision br net
    where
        br = ballRect ball
        onCollision :: Vector2 Double -> Ball
        onCollision v = Ball (Rect p' s) (reflect (0.4 *^ bv) n)
            where
                p' = v ^+^ bp
                (Rect bp s) = ballRect ball
                bv = ballVelocity ball
                n = normalize v

game :: SF GameInput Game
game = gameSession (0, 0)

gameSession :: Scores -> SF GameInput Game
gameSession (score1, score2) = switch sf (gameSession . incrementScores (score1, score2))
    where
        sf = proc (GameInput restart (mdir1, mdir2)) -> do
            -- r <- tag Nothing ^<< edge -< restart
            p1 <- clampedPaddle (-1000, 0) paddle1Start -< mdir1
            p2 <- clampedPaddle (0, 1000) paddle2Start -< mdir2
            b <- ball initialBall -< (p1, p2)
            ballOut <- (arr (\b -> if checkBallOutside b then Event b else NoEvent)) -< b
            returnA -< (Game { gamePaddle1 = p1, gamePaddle2 = p2, gameBall = b, gameScores = (score1, score2) }, fmap winnerPlayer ballOut)
            where
                paddle1Start = Paddle $ Rect (vector2 (-200 - x paddleSize) 10) paddleSize
                paddle2Start = Paddle $ Rect (vector2 200 10) paddleSize
                checkBallOutside :: Ball -> Bool
                checkBallOutside b = y' < 0
                    where
                        y' = y bp
                        (Rect bp _) = ballRect b
                initialBall = Ball
                    { 
                        ballRect = Rect (vector2 (-10) 400) (vector2 20 20),
                        ballVelocity = vector2 (-200) 0
                    }

incrementScores :: Scores -> Player -> Scores
incrementScores (x, y) (Player 1) = (x + 1, y)
incrementScores (x, y) (Player 2) = (x, y + 1)
incrementScores s _ = s

data Player = Player Int deriving (Show, Eq)

-- assuming the ball is outside
winnerPlayer :: Ball -> Player
winnerPlayer b = if ballInsideField b then side else otherPlayer side
    where
        otherPlayer (Player 1) = Player 2
        otherPlayer (Player 2) = Player 1
        side = ballPlayerSide b

ballPlayerSide :: Ball -> Player
ballPlayerSide b = if bx < 0 then Player 1 else Player 2
    where
        bx = x $ center $ ballRect $ b

ballInsideField :: Ball -> Bool
ballInsideField = not . isOverlapping fieldRect . ballRect
    where
        fieldRect = Rect (vector2 (-fieldWidth / 2) (-1000000)) (vector2 fieldWidth 20000000)
