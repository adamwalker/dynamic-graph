{-| Various utilities for drawing axes with Cairo that will be later
    rendered using @Graphics.DynamicGraph.RenderCairo@
-}

{-# LANGUAGE RecordWildCards #-}

module Graphics.DynamicGraph.Axis (
    blankCanvas,
    blankCanvasAlpha,
    drawAxes,
    gridXCoords,
    gridYCoords,
    xAxisLabels,
    yAxisLabels,
    xAxisGrid,
    yAxisGrid
    ) where

import Control.Monad
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Colour.Names
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

-- Make a pango layout, fill it with text and return its extents
makeLayout :: PangoContext -> String -> Render (PangoLayout, PangoRectangle)
makeLayout ctx text = liftIO $ do
    layout <- layoutEmpty ctx
    layoutSetMarkup layout text :: IO String
    (_, rect) <- layoutGetExtents layout
    return (layout, rect)

layoutTopCentre :: PangoContext -> String -> Double -> Double -> Render ()
layoutTopCentre ctx text x y = do
    (layout, PangoRectangle _ _ w _) <- makeLayout ctx text
    moveTo (x - w/2) y
    showLayout layout

layoutRightCentre :: PangoContext -> String -> Double -> Double -> Render ()
layoutRightCentre ctx text x y = do
    (layout, PangoRectangle _ _ w h) <- makeLayout ctx text
    moveTo (x - w) (y - h/2)
    showLayout layout

blankCanvas :: Colour Double -> Double -> Double -> Render ()
blankCanvas colour width height  = do
    uncurryRGB setSourceRGB (toSRGB colour)
    rectangle 0 0 width height
    fill

blankCanvasAlpha :: Colour Double -> Double -> Double -> Double -> Render ()
blankCanvasAlpha colour alpha width height  = do
    uncurryRGB (\x y z -> setSourceRGBA x y z alpha) (toSRGB colour)
    rectangle 0 0 width height
    fill

drawAxes :: Double -> Double -> Double -> Double -> Double -> Double -> Colour Double -> Double -> Render ()
drawAxes width height topMargin bottomMargin leftMargin rightMargin axisColor axisWidth = do
    setDash [] 0
    setLineCap  LineCapRound
    setLineJoin LineJoinRound
    setLineWidth axisWidth
    uncurryRGB setSourceRGB (toSRGB axisColor)

    --Y axis
    moveTo leftMargin topMargin
    lineTo leftMargin (height - bottomMargin)
    stroke

    --X axis
    moveTo leftMargin (height - bottomMargin)
    lineTo (width - rightMargin) (height - bottomMargin)
    stroke

gridXCoords :: Double -> Double -> Double -> Double -> Double -> [Double]
gridXCoords width offset leftMargin rightMargin spacing = takeWhile (<= (width - rightMargin)) $ iterate (+ spacing) (offset + leftMargin)

gridYCoords :: Double -> Double -> Double -> Double -> Double -> [Double]
gridYCoords height offset topMargin bottomMargin spacing = takeWhile (>= topMargin) $ iterate (flip (-) spacing) (height - bottomMargin - offset)

xAxisLabels :: PangoContext -> Colour Double -> [String] -> [Double] -> Double -> Render ()
xAxisLabels ctx textColor gridLabels gridXCoords yCoord = do
    uncurryRGB setSourceRGB (toSRGB textColor)
    forM_ (zip gridLabels gridXCoords) $ \(label, xCoord) -> do
        layoutTopCentre ctx label xCoord yCoord

yAxisLabels :: PangoContext -> Colour Double -> [String] -> [Double] -> Double -> Render ()
yAxisLabels ctx textColor gridLabels gridYCoords xCoord = do
    uncurryRGB setSourceRGB (toSRGB textColor)
    forM_ (zip gridLabels gridYCoords) $ \(label, yCoord) -> do
        layoutRightCentre ctx label xCoord yCoord

xAxisGrid :: Colour Double -> Double -> [Double] -> Double -> Double -> [Double] -> Render ()
xAxisGrid gridColor gridWidth gridDash yStart yEnd gridXCoords = do
    uncurryRGB setSourceRGB (toSRGB gridColor)
    setLineWidth gridWidth
    setDash gridDash 0
    forM_ gridXCoords $ \xCoord -> do
        moveTo xCoord yStart
        lineTo xCoord yEnd
        stroke

yAxisGrid :: Colour Double -> Double -> [Double] -> Double -> Double -> [Double] -> Render ()
yAxisGrid gridColor gridWidth gridDash xStart xEnd gridYCoords = do
    uncurryRGB setSourceRGB (toSRGB gridColor)
    setLineWidth gridWidth
    setDash gridDash 0
    forM_ gridYCoords $ \yCoord -> do
        moveTo xStart yCoord
        lineTo xEnd   yCoord
        stroke

