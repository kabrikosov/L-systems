{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Turtle where

import Monomer
import Control.Lens
import Data.Text (Text, pack, unpack)
import qualified Monomer.Lens as L

(#+#) :: Point -> Point -> Point
(#+#) (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

(#-#) :: Point -> Point -> Point
(#-#) (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)

(#/#) :: Point -> Point -> Point
(#/#) (Point x1 y1) (Point x2 y2) = Point ((x1 + x2)/2) ((y1 + y2)/2)

(#/) :: Point -> Double -> Point
(#/) (Point x1 y1) m = Point (x1/m) (y1/m)

data Turtle = Turtle {
  _position :: Point,
  _angle :: Double,
  _states :: [Turtle]
} deriving (Eq, Show)

data Line = Line {
  _a :: Point,
  _b :: Point
} deriving (Eq, Show)

makeLenses 'Turtle

move :: Turtle -> Double -> Turtle
move (Turtle (Point x y) a states) dist = Turtle (Point (x + dist * (cos a)) (y + dist * (sin a))) a states

rotate :: Turtle -> Double -> Turtle
rotate (Turtle a b states) rot = Turtle a (b + (fromDegrees rot)) states

getPosition :: Turtle -> Point
getPosition (Turtle p a states) = p

saveState :: Turtle -> Turtle
saveState (Turtle a b states) = Turtle a b ([Turtle a b states] ++ states)

getLastState :: Turtle -> Turtle
getLastState (Turtle a b []) = Turtle a b []
getLastState (Turtle a b (x:xs)) = x

out :: Int -> String -> String -> String -> String -> String
out 0 start ruleA ruleB ruleC = start
out 1 start ruleA ruleB ruleC = _repl start ruleA ruleB ruleC ""
out n start ruleA ruleB ruleC = out (n-1) (_repl start ruleA ruleB ruleC "") ruleA ruleB ruleC

_repl :: String -> String -> String -> String -> String -> String
_repl [] ruleA ruleB ruleC s = s
_repl (x:xs) ruleA ruleB ruleC s | x == 'A' = _repl (xs) ruleA ruleB ruleC (s ++ ruleA)
                                 | x == 'B' = _repl (xs) ruleA ruleB ruleC (s ++ ruleB)
                                 | x == 'C' = _repl (xs) ruleA ruleB ruleC (s ++ ruleC)
                                 | otherwise = _repl (xs) ruleA ruleB ruleC (s++[x])

getPoints :: Int -> String -> String -> String -> String -> Double -> [Point]
getPoints d s a b c ang = normalizePoints $ fromLines $ getLiness d s a b c ang

normalizePoints :: [Point] -> [Point]
normalizePoints pts = result where
  oX = minimum (map (\(Point x y) -> x) pts)
  oY = minimum (map (\(Point x y) -> y) pts)
  result = map (\(Point x y) -> Point (x - oX) (y - oY)) pts

-- getPoints' :: Turtle -> Double -> Double -> String -> [Point] -> [Point]
-- getPoints' t a step [] x = x
-- getPoints' t a step x [] = getPoints' t a step x [getPosition t]
-- getPoints' t a step (x:xs) r | x == '[' = getPoints' (saveState t) a step xs r
--                              | x == ']' = getPoints' (getLastState t) a step xs (r ++ [getPosition t] ++[getPosition $ getLastState t])
--                              | x == 'A' = getPoints' (move t step) a step xs (r ++ [getPosition $ move t step])
--                              | x == 'B' = getPoints' (move t step) a step xs (r ++ [getPosition $ move t step])
--                              | x == '+' = getPoints' (rotate t a) a step xs r
--                              | x == '-' = getPoints' (rotate t (-a)) a step xs r
--                              | otherwise = getPoints' t a step xs r

getLiness :: Int -> String -> String -> String -> String -> Double -> [Line]
getLiness d s a b c ang = getLiness' (Turtle (Point 0 0) (fromDegrees 270) []) ang 10 (out d s a b c) []

getLiness' :: Turtle -> Double -> Double -> String -> [Line] -> [Line]
getLiness' t a step [] x = x
getLiness' t a step (x:xs) r | x == '[' = getLiness' (saveState t) a step xs r
                             | x == ']' = getLiness' (getLastState t) a step xs r
                             | x == 'A' = getLiness' (move t step) a step xs (r ++ [Line (getPosition t) (getPosition $ move t step)])
                             | x == 'B' = getLiness' (move t step) a step xs (r ++ [Line (getPosition t) (getPosition $ move t step)])
                             | x == 'C' = getLiness' t a step xs r
                             | x == '+' = getLiness' (rotate t a) a step xs r
                             | x == '-' = getLiness' (rotate t (-a)) a step xs r
                             | otherwise = getLiness' t a step xs r

fromLines :: [Line] -> [Point]
fromLines x = fromLines' x []

fromLines' :: [Line] -> [Point] -> [Point]
fromLines' [] x = x
fromLines' (x:xs) r = fromLines' xs (r ++ ((\(Line a b) -> [a, b]) x))

fromDegrees :: Double -> Double
fromDegrees a = a * pi / 180
