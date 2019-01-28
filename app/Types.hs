module Types where

import FRP.Yampa.Vector2

type Position = Vector2 Double
type Velocity = Vector2 Double
type Size = Vector2 Double
    
x = vector2X :: Vector2 Double -> Double
y = vector2Y :: Vector2 Double -> Double


clamp :: (Ord a) => a -> a -> a -> a
clamp x y = max x . min y

clamp01 :: (Num a, Ord a) => a -> a
clamp01 = clamp 0 1