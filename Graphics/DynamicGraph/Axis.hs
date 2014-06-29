{-# LANGUAGE RecordWildCards #-}

module Graphics.DynamicGraph.Axis where

import Control.Monad
import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Colour.Names
import Graphics.Rendering.Cairo
import Graphics.Rendering.Pango

data Configuration = Configuration {
    width           :: Double,
    height          :: Double,
    topMargin       :: Double,
    bottomMargin    :: Double,
    leftMargin      :: Double,
    rightMargin     :: Double,

    backgroundColor :: Colour Double,
    axisColor       :: Colour Double,
    axisWidth       :: Double,
    xGridConfig     :: Maybe GridConfig,
    yGridConfig     :: Maybe GridConfig
}

data GridConfig = GridConfig {
    gridColor   :: Colour Double,
    gridWidth   :: Double,
    gridDash    :: [Double],
    gridOffset  :: Double,
    gridSpacing :: Double,
    gridLabels  :: [String],
    textColor   :: Colour Double
}

defaultXGridConfig = GridConfig {..}
    where
    gridColor   = gray
    gridWidth   = 0.5
    gridDash    = []
    gridOffset  = 50
    gridSpacing = 100
    gridLabels  = ["l1", "l2", "l3", "l4", "l5", "l6"]
    textColor   = white

defaultYGridConfig = GridConfig {..}
    where
    gridColor   = gray
    gridWidth   = 0.5
    gridDash    = [3, 1.5]
    gridOffset  = 0
    gridSpacing = 50
    gridLabels  = ["l1", "l2", "l3", "l4", "l5", "l6"]
    textColor   = white

defaultConfiguration = Configuration {..}
    where
    width           = 500
    height          = 500
    topMargin       = 50
    bottomMargin    = 50
    leftMargin      = 50
    rightMargin     = 50

    backgroundColor = black
    axisColor       = lightgray
    axisWidth       = 1
    xGridConfig     = Just defaultXGridConfig
    yGridConfig     = Just defaultYGridConfig

whenMaybe :: (Functor m, Monad m) => Maybe a -> (a -> m b) -> m ()
whenMaybe mb func = maybe (return ()) (void . func) mb

renderAxes Configuration{..} = do
    --set the background colour
    uncurryRGB setSourceRGB (toSRGB backgroundColor)
    rectangle 0 0 width height
    fill

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

    ctx <- liftIO $ cairoCreateContext Nothing

    --X grid
    whenMaybe xGridConfig $ \GridConfig{..} -> do
        forM (zip gridLabels $ takeWhile (< (width - rightMargin)) $ iterate (+ gridSpacing) (gridOffset + leftMargin)) $ \(label, xCoord) -> do
            --grid lines
            uncurryRGB setSourceRGB (toSRGB gridColor)
            setLineWidth gridWidth
            setDash gridDash 0
            moveTo xCoord (height - bottomMargin)
            lineTo xCoord topMargin
            stroke

            --axis labels
            layout <- liftIO $ do
                layout <- layoutEmpty ctx
                layoutSetMarkup layout label
                return layout
            (_, PangoRectangle _ _ w _) <- liftIO $ layoutGetExtents layout
            moveTo (xCoord - w/2) (height - bottomMargin)
            uncurryRGB setSourceRGB (toSRGB textColor)
            showLayout layout

    --Y grid
    whenMaybe yGridConfig $ \GridConfig{..} -> do
        forM (zip gridLabels $ takeWhile (> topMargin) $ iterate (flip (-) gridSpacing) (height - bottomMargin - gridOffset)) $ \(label, yCoord) -> do
            --grid lines
            uncurryRGB setSourceRGB (toSRGB gridColor)
            setLineWidth gridWidth
            setDash gridDash 0
            moveTo (width - rightMargin) yCoord
            lineTo leftMargin yCoord
            stroke

            --axis labels
            layout <- liftIO $ do
                layout <- layoutEmpty ctx
                layoutSetMarkup layout label
                return layout
            (_, PangoRectangle _ _ w h) <- liftIO $ layoutGetExtents layout
            moveTo (50 - w) (yCoord - h/2)
            uncurryRGB setSourceRGB (toSRGB textColor)
            showLayout layout

{-
    layout <- liftIO $ do
        layout <- layoutEmpty ctx
        layoutSetMarkup layout "Title"
        return layout
    (_, PangoRectangle _ _ w h) <- liftIO $ layoutGetExtents layout
    moveTo ((width - w) / 2) 0
    showLayout layout
-}

