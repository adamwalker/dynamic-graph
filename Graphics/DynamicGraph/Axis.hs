{-| Various utilities for drawing axes with Cairo that will later be rendered using @Graphics.DynamicGraph.RenderCairo@
-}

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

-- | Make a pango layout, fill it with text and return its extents
makeLayout :: PangoContext -- ^ Pango context
           -> String       -- ^ The text
           -> Render (PangoLayout, PangoRectangle)
makeLayout ctx text = liftIO $ do
    layout <- layoutEmpty ctx
    layoutSetMarkup layout text :: IO String
    (_, rect) <- layoutGetExtents layout
    return (layout, rect)

-- | Draw some text in the top center 
layoutTopCentre :: PangoContext -- ^ Pango context
                -> String       -- ^ The text
                -> Double       -- ^ Width
                -> Double       -- ^ Height
                -> Render ()
layoutTopCentre ctx text x y = do
    (layout, PangoRectangle _ _ w _) <- makeLayout ctx text
    moveTo (x - w/2) y
    showLayout layout

-- | Draw some text right side half way up
layoutRightCentre :: PangoContext -- ^ Pango context 
                  -> String       -- ^ The text
                  -> Double       -- ^ Width
                  -> Double       -- ^ Height
                  -> Render ()
layoutRightCentre ctx text x y = do
    (layout, PangoRectangle _ _ w h) <- makeLayout ctx text
    moveTo (x - w) (y - h/2)
    showLayout layout

-- | Create a blank cairo canvas of the specified size and colour
blankCanvas :: Colour Double -- ^ The colour
            -> Double        -- ^ Width
            -> Double        -- ^ Height
            -> Render ()
blankCanvas colour width height  = do
    uncurryRGB setSourceRGB (toSRGB colour)
    rectangle 0 0 width height
    fill

-- | Create a blank cairo canvas of the specified size and colour
blankCanvasAlpha :: Colour Double -- ^ The colour
                 -> Double        -- ^ Transparency
                 -> Double        -- ^ Width
                 -> Double        -- ^ Height
                 -> Render ()
blankCanvasAlpha colour alpha width height  = do
    uncurryRGB (\x y z -> setSourceRGBA x y z alpha) (toSRGB colour)
    rectangle 0 0 width height
    fill

-- | Draw a set of axes without any labels
drawAxes :: Double        -- ^ Width
         -> Double        -- ^ Height
         -> Double        -- ^ Top Margin
         -> Double        -- ^ Bottom Margin
         -> Double        -- ^ Left Margin
         -> Double        -- ^ Right Margin
         -> Colour Double -- ^ Axis colour
         -> Double        -- ^ Axis width
         -> Render ()
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

-- | Calculate the coordinates to draw the X axis grid at
gridXCoords :: Double -- ^ Width of graph
            -> Double -- ^ X offset to start at
            -> Double -- ^ Left margin
            -> Double -- ^ Right margin
            -> Double -- ^ Spacing between coordinates
            -> [Double]
gridXCoords width offset leftMargin rightMargin spacing = takeWhile (<= (width - rightMargin)) $ iterate (+ spacing) (offset + leftMargin)

-- | Calculate the coordinates to draw the Y axis grid at
gridYCoords :: Double -- ^ Height of graph
            -> Double -- ^ Y offset to start at
            -> Double -- ^ Top margin
            -> Double -- ^ Bottom margin
            -> Double -- ^ Spacing between coordinates
            -> [Double]
gridYCoords height offset topMargin bottomMargin spacing = takeWhile (>= topMargin) $ iterate (flip (-) spacing) (height - bottomMargin - offset)

-- | Draw X axis labels
xAxisLabels :: PangoContext  -- ^ Pango context
            -> Colour Double -- ^ Label colour
            -> [String]      -- ^ Grid labels
            -> [Double]      -- ^ X coordinates to draw labels at
            -> Double        -- ^ Y coordinate to draw labels at
            -> Render ()
xAxisLabels ctx textColor gridLabels gridXCoords yCoord = do
    uncurryRGB setSourceRGB (toSRGB textColor)
    forM_ (zip gridLabels gridXCoords) $ \(label, xCoord) -> 
        layoutTopCentre ctx label xCoord yCoord

-- | Draw Y axis labels
yAxisLabels :: PangoContext  -- ^ Pango context
            -> Colour Double -- ^ Label colour
            -> [String]      -- ^ Grid label
            -> [Double]      -- ^ Y coordinates to draw labels at
            -> Double        -- ^ X coordinate to draw labels at
            -> Render ()
yAxisLabels ctx textColor gridLabels gridYCoords xCoord = do
    uncurryRGB setSourceRGB (toSRGB textColor)
    forM_ (zip gridLabels gridYCoords) $ \(label, yCoord) -> 
        layoutRightCentre ctx label xCoord yCoord

-- | Draw X axis grid
xAxisGrid :: Colour Double -- ^ Grid colour
          -> Double        -- ^ Width of grid lines
          -> [Double]      -- ^ Grid line dashing
          -> Double        -- ^ Starting Y coordinate
          -> Double        -- ^ Ending Y coordinate
          -> [Double]      -- ^ Grid X coordinates
          -> Render ()
xAxisGrid gridColor gridWidth gridDash yStart yEnd gridXCoords = do
    uncurryRGB setSourceRGB (toSRGB gridColor)
    setLineWidth gridWidth
    setDash gridDash 0
    forM_ gridXCoords $ \xCoord -> do
        moveTo xCoord yStart
        lineTo xCoord yEnd
        stroke

-- | Draw Y axis grid
yAxisGrid :: Colour Double -- ^ Grid color
          -> Double        -- ^ Width of grid lines
          -> [Double]      -- ^ Grid line dashing
          -> Double        -- ^ Starting X coordinate
          -> Double        -- ^ Ending X coordinate
          -> [Double]      -- ^ Grid Y coordinates
          -> Render ()
yAxisGrid gridColor gridWidth gridDash xStart xEnd gridYCoords = do
    uncurryRGB setSourceRGB (toSRGB gridColor)
    setLineWidth gridWidth
    setDash gridDash 0
    forM_ gridYCoords $ \yCoord -> do
        moveTo xStart yCoord
        lineTo xEnd   yCoord
        stroke

