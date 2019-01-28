module Rect where

import FRP.Yampa.Vector2
import FRP.Yampa.VectorSpace
import Types
import Debug.Trace

data Rect = Rect Position Size deriving (Eq, Show) 

rectPos :: Rect -> Position
rectPos (Rect p _) = p

rectSize :: Rect -> Position
rectSize (Rect _ s) = s

isOverlapping :: Rect -> Rect -> Bool
isOverlapping (Rect p1 s1) (Rect p2 s2) =
    x p1 < x p2 + x s2 &&
    x p1 + x s1 > x p2 &&
    y p1 < y p2 + y s2 &&
    y p1 + y s1 > y p2

overlap :: Rect -> Rect -> Maybe (Vector2 Double)
overlap (Rect p1 s1) (Rect p2 s2) = do
    dx1 <- x p1 <| (x p2 + x s2)
    dx2 <- (x p1 + x s1) >| x p2
    dy1 <- y p1 <| (y p2 + y s2)
    dy2 <- (y p1 + y s1) |> y p2
    return $ vector2 (minAbs dx1 dx2) (minAbs dy1 dy2)
    where
        minAbs :: Double -> Double -> Double
        minAbs x y = if abs x < abs y then x else y

collision :: Rect -> Rect -> Maybe (Vector2 Double)
collision r1 r2 = do
    v <- overlap r1 r2
    return $ if abs (x v) < abs (y v) then vector2 (x v) 0 else vector2 0 (y v)

collide :: Rect -> Rect -> Maybe Rect
collide r1 r2 = do
    v <- collision r1 r2
    let Rect p s = r2
    return $ Rect (p ^+^ v) s

center :: Rect -> Position
center (Rect p s) = p ^+^ (s ^/ 2)

size :: Rect -> Vector2 Double
size (Rect _ s) = s

(~>) :: a -> (a -> b) -> b
(~>) = flip ($)
        
(|<) :: (Ord a, Num a) => a -> a -> Maybe a
x |< y = if x < y then Just (x - y) else Nothing

(|>) :: (Ord a, Num a) => a -> a -> Maybe a
x |> y = if x > y then Just (x - y) else Nothing

(<|) :: (Ord a, Num a) => a -> a -> Maybe a
x <| y = if x < y then Just (y - x) else Nothing

(>|) :: (Ord a, Num a) => a -> a -> Maybe a
x >| y = if x > y then Just (y - x) else Nothing
