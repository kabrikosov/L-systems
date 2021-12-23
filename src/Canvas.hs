{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Canvas where

import Control.Concurrent (threadDelay, forkIO)
import Control.Lens
import Control.Monad (forM_)
import Data.Default
import Data.Text (Text)
import Data.Typeable (cast)
import Data.List.Split (splitEvery)
import Monomer
import Monomer.Widgets.Single
import Turtle
import Data.List (sort)

import qualified Data.Text as T
import qualified Monomer.Lens as L

newtype CanvasCfg = CanvasCfg {
  _canvasColor :: Color
} deriving (Eq, Show)

instance Default CanvasCfg where
  def = CanvasCfg {
    _canvasColor = white
  }

data CanvasMessage
  = UpdateCanvasWith Color Int String String String String Double Double
  deriving (Eq, Show)

data CanvasState = CanvasState {
  _clickedPoints :: [Point],
  _canvasZoomFactor :: Double,
  _movePosition :: Point,
  _lineWeight :: Double
} deriving (Eq, Show)

makeLenses 'CanvasCfg
makeLenses 'CanvasState

canvas :: Color -> Int -> String -> String -> String -> String -> Double -> Double -> WidgetNode s e
canvas c d s a b rc ang w = canvas_ c d s a b rc ang w

canvas_ :: Color -> Int -> String -> String -> String -> String -> Double -> Double -> WidgetNode s e
canvas_ c d s a b rc ang w = defaultWidgetNode "canvas" newWidget where
  state = CanvasState (getPoints d s a b rc ang) 1 (Point 0 0) w
  newWidget = makeCanvas c state

makeCanvas :: Color -> CanvasState -> Widget s e
makeCanvas c state = widget where
  widget = createSingle state def {
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleHandleMessage = handleMessage,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  color = c

  merge wenv node oldNode oldState = result where
    newNode = node
      & L.widget .~ makeCanvas color oldState
    result = resultNode newNode

  handleEvent wenv node target evt = case  evt of
    KeyAction mode code (KeyPressed) -> Just result where
      movex | code == keyRight  = 10
            | code == keyLeft  = -10
            | otherwise = 0
      movey | code == keyDown = 10
            | code == keyUp = -10
            | otherwise = 0
      newState = CanvasState (state ^. clickedPoints) (state^.canvasZoomFactor) ((state^.movePosition) #+# (Point movex movey)) (state^.lineWeight)
      newNode = node
        & L.widget .~ makeCanvas color newState
      result = resultNode newNode

    WheelScroll pt1 pt2 (dir) -> Just result where
      vp = node ^. L.info . L.viewport
      center = (Point (vp^.L.x) (vp^.L.y)) #/# (Point (vp^.L.w) (vp^.L.h))
      newState = CanvasState (state^.clickedPoints) (state^.canvasZoomFactor + (pt2^.L.y)/100) ((state^.movePosition) #-# ((center #-# pt1) #/100)) (state^.lineWeight)
      newNode = node
        & L.widget .~ makeCanvas color newState
      result = resultNode newNode

    _ -> Nothing

  handleMessage wenv node target msg = case cast msg of
    Just (UpdateCanvasWith c d s a b rc ang w) -> Just result where
      color = c
      vp = node ^. L.info . L.viewport
      l = getPoints d s a b rc ang
      center = (Point (vp^.L.x) (vp^.L.y)) #/# (Point (vp^.L.w) (vp^.L.h))
      coords = foldr (++) [] $ map (\(Point a b) -> [a, b]) l
      newZoomFactor = maximum coords
      newState | 0 == (state^.canvasZoomFactor) = CanvasState (l) newZoomFactor (center #-# (Point (newZoomFactor/2) 0)) w
               | otherwise = CanvasState (l) (state^.canvasZoomFactor) (state^.movePosition) w
      newNode = node
        & L.widget .~ makeCanvas c newState
      result = resultNode newNode
    _ -> Nothing

  getSizeReq wenv node = (sizeReqW, sizeReqH) where
    sizeReqW = minWidth 100
    sizeReqH = minHeight 100

  render wenv node renderer = do
    drawInScissor renderer True vp $
      drawInTranslation renderer origin $
        drawInScale renderer rect $
          forM_ tuples $ \(pointA, pointB) -> do
            setStrokeColor renderer color
            setStrokeWidth renderer (state^.lineWeight)
            beginPath renderer
            renderLine renderer pointA pointB
            stroke renderer
        where
          vp = node ^. L.info . L.viewport
          coords = foldr (++) [] $ map (\(Point a b) -> [a, b]) points
          wh = Point (vp^.L.w) (vp^.L.h)
          minimal = min ((\(Point a b) -> b) wh) ((\(Point a b) -> a) wh)
          maximal = max ((\(Point a b) -> b) wh) ((\(Point a b) -> a) wh)
          xes = map (\(Point a b) -> a) points
          yes = map (\(Point a b) -> b) points
          center = (Point (vp^.L.x+400) (vp^.L.y)) #/# (Point ((vp^.L.w)) (vp^.L.h)) #-# (Point (maximal/3) (maximal/3))
          origin = (center) #+# (state^.movePosition)
          rect | coords == [] = Point (vp^.L.x) (vp^.L.y)
               | otherwise = Point ((state^.canvasZoomFactor)*(minimal)/(maximum coords)) ((state^.canvasZoomFactor)*(minimal)/(maximum coords))
          clicked = state ^. clickedPoints
          points = clicked
          tuples = map (\[a, b]->(a, b)) $ splitEvery 2 clicked
