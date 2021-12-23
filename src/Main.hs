{-|
Module      : Tutorial07_CustomWidget
Copyright   : (c) 2018 Francisco Vallarino
License     : BSD-3-Clause (see the LICENSE file)
Maintainer  : fjvallarino@gmail.com
Stability   : experimental
Portability : non-portable
Main module for the '07 - Custom Widget' tutorial.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Lens
import Control.Monad (forM_)
import Data.Default
import Data.Fixed (mod')
import Data.Text (Text, pack, unpack)
import Data.Typeable (cast)
import Monomer
import Monomer.Widgets.Single
import Turtle
import Canvas

import qualified Data.Text as T
import qualified Monomer.Lens as L

data AppModel = AppModel{
  _cfgRuleA :: Text,
  _cfgRuleB :: Text,
  _cfgRuleC :: Text,
  _cfgStart :: Text,
  _cfgAngle :: Double,
  _cfgDepth :: Int,
  _cfgColor :: Color,
  _cfgShowPicker :: Bool,
  _cfgAnimation :: Bool,
  _cfgLineWeight :: Double,
  _cfgAnimationSpeed :: Int
}
  deriving (Eq, Show)

data AppEvent
  = AppInit
  |AppUpdateCanvas
  | AppRunAnimation
  | AppUpdateModel
  | AppStopAnimation
  | AppRestartAnimation
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = hstack [
    vstack [
        hstack [
          vstack [
              titleText "Start",
              spacer,
              box (textField cfgStart) `styleBasic` [textCenter]
          ] `styleBasic` [paddingH 10],

          vstack [
              titleText "Rule for A",
              spacer,
              box (textField cfgRuleA) `styleBasic` [textCenter]
          ] `styleBasic` [paddingH 10],

          vstack [
              titleText "Rule for B",
              spacer,
              box (textField cfgRuleB) `styleBasic` [textCenter]
          ] `styleBasic` [paddingH 10]

        ] `styleBasic` [paddingV 10],

        vstack [
            titleText "Rule for C",
            spacer,
            box (textField cfgRuleC) `styleBasic` [textCenter]
        ] `styleBasic` [paddingH 10],

        hstack [
          vstack[
            titleText "Angle",
            spacer,
            box (numericField cfgAngle) `styleBasic` [textCenter]
          ] `styleBasic` [paddingH 10],

          vstack[
            titleText "Depth",
            spacer,
            box (numericField cfgDepth) `styleBasic` [textCenter]
          ] `styleBasic` [paddingH 10],

          vstack[
            titleText "Line weight",
            spacer,
            box (numericField cfgLineWeight) `styleBasic` [textCenter]
          ] `styleBasic` [paddingH 10]

        ] `styleBasic` [paddingV 10],

        hstack[
          vstack [
            titleText "Line color",
            hstack [
              labeledCheckbox "Show color picker " cfgShowPicker,
              filler
            ] `styleBasic` [paddingT 10, paddingB 5],

            colorPicker cfgColor
              `nodeVisible` (model ^. cfgShowPicker)
              `styleBasic` [paddingB 10],

            vstack [
              button "Draw!" AppUpdateCanvas,
              spacer,
              button "Animate" AppRunAnimation,
              spacer,
              button "Stop" AppStopAnimation
            ] `styleBasic` [paddingV 10]

          ] `styleBasic` [paddingH 10]

        ] `styleBasic` [paddingV 10]
      ] `styleBasic` [maxWidth 400],

      animFadeOut (
        (canvas
          (model^.cfgColor)
          (model^.cfgDepth)
          (unpack $ model^.cfgStart)
          (unpack $ model^.cfgRuleA)
          (unpack $ model^.cfgRuleB)
          (unpack $ model^.cfgRuleC)
          (model^.cfgAngle)
          (model^.cfgLineWeight)
        ) `nodeKey` "mainCanvas" `styleBasic` [border 1 gray]
      ) `nodeKey` "animCanvas"
    ] `styleBasic` [padding 10]


  titleText text = label text
      `styleBasic` [textFont "Medium", textSize 20, textCenter]


handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [Producer canvasAnimationProducer]
  AppUpdateCanvas -> result where
    result = [Message "mainCanvas" (UpdateCanvasWith
                                      (model^.cfgColor)
                                      (model^.cfgDepth)
                                      (unpack $ model^.cfgStart)
                                      (unpack $ model^.cfgRuleA)
                                      (unpack $ model^.cfgRuleB)
                                      (unpack $ model^.cfgRuleC)
                                      (model^.cfgAngle)
                                      (model^.cfgLineWeight)
                                    ), SetFocusOnKey "mainCanvas"]
  AppRunAnimation -> [Model $ model & cfgAnimation .~ True, SetFocusOnKey "mainCanvas"]
  AppRestartAnimation -> [Message "animCanvas" AnimationStart]
  AppStopAnimation -> [Model $ model & cfgAnimation .~ False, Message "animCanvas" AnimationStop]
  AppUpdateModel -> result where
    result | (model^.cfgAnimation) = [Model $ model & cfgAngle .~ ((model^.cfgAngle + 0.2) `mod'` 360),
                                      Message "mainCanvas" (UpdateCanvasWith
                                                                      (model^.cfgColor)
                                                                      (model^.cfgDepth)
                                                                      (unpack $ model^.cfgStart)
                                                                      (unpack $ model^.cfgRuleA)
                                                                      (unpack $ model^.cfgRuleB)
                                                                      (unpack $ model^.cfgRuleC)
                                                                      (model^.cfgAngle)
                                                                      (model^.cfgLineWeight)
                                                                    )]
           | otherwise = []

canvasAnimationProducer :: (AppEvent -> IO ()) -> IO ()
canvasAnimationProducer sendMsg = do
  threadDelay $ 1000 * 10
  sendMsg AppUpdateModel
  sendMsg AppRestartAnimation
  canvasAnimationProducer sendMsg

main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "L-systems",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
      appInitEvent AppInit
      ]
    model = AppModel {
    _cfgRuleA = "AA",
    _cfgRuleB = "",
    _cfgRuleC = "A-[[C]+C]+A[+AC]-C",
    _cfgStart = "C",
    _cfgAngle = 25,
    _cfgDepth = 5,
    _cfgColor = white,
    _cfgShowPicker = False,
    _cfgAnimation = False,
    _cfgLineWeight = 2,
    _cfgAnimationSpeed = 1
  }
