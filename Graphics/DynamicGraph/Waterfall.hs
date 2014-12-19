{-# LANGUAGE ScopedTypeVariables #-}
{-| Draw and update waterfall plots with OpenGL. Useful for spectrograms.
-}
module Graphics.DynamicGraph.Waterfall (
    jet,
    jet_mod,
    hot,
    bw,
    wb,
    waterfallWindow,
    renderWaterfall
    ) where

import Control.Monad
import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array
import Data.IORef

import Pipes

import Graphics.DynamicGraph.Util

import Paths_dynamic_graph

-- | The matlab / octave \"jet\" color map
jet :: [GLfloat]
jet =  [0, 0, 0.5,  0, 0, 1,  0, 0.5, 1,   0, 1, 1,  0.5, 1, 0.5,  1, 1, 0,  1, 0.5, 0,  1, 0, 0,  0.5, 0, 0]

-- | \"jet\" modified so that low values are a darker blue
jet_mod :: [GLfloat]
jet_mod =  [0, 0, 0.1,  0, 0, 1,  0, 0.5, 1,   0, 1, 1,  0.5, 1, 0.5,  1, 1, 0,  1, 0.5, 0,  1, 0, 0,  0.5, 0, 0]

-- | The matlab / octave \"hot\" color map
hot :: [GLfloat]
hot =  [0, 0, 0,  1, 0, 0,  1, 1, 0,  1, 1, 1]

-- | Ranges from black to white
bw :: [GLfloat]
bw =  [0, 0, 0, 1, 1, 1]

-- | Ranges from white to black
wb :: [GLfloat]
wb =  [1, 1, 1, 0, 0, 0]

{-| @(waterfallWindow windowWidth windowHeight width height colormap)@
    creates a window of width @windowWidth@ and height @windowHeight@ for
    displaying a waterfall plot. 
    
    A Consumer is returned for updating the waterfall plot. Feeding an
    instance of IsPixelData of length @width@ shifts all rows of the
    waterfall down and updates the top row with the data. 
        
    The waterfall is @height@ rows of data high. @colorMap@ is used to map
    values to display color.
-}
waterfallWindow :: IsPixelData a => Int -> Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer a IO ())
waterfallWindow windowWidth windowHeight width height colorMap = do
    mv :: MVar a <- lift $ newEmptyMVar
    completion <- lift $ newEmptyMVar

    closed <- lift $ newIORef False

    lift $ forkOS $ void $ do
        res <- runEitherT $ do
            res' <- lift $ createWindow windowWidth windowHeight "" Nothing Nothing
            win <- maybe (left "error creating window") return res'
            lift $ setWindowSizeCallback win $ Just $ \win x y -> do
                viewport $= (Position 0 0, Size (fromIntegral x) (fromIntegral y))
            lift $ setWindowCloseCallback win $ Just $ \win -> writeIORef closed True
            lift $ makeContextCurrent (Just win)
            renderPipe <- lift $ renderWaterfall width height colorMap
            let thePipe = forever $ do 
                    lift $ pollEvents
                    dat <- lift $ takeMVar mv
                    lift $ makeContextCurrent (Just win)
                    lift $ pollEvents
                    yield dat
                    lift $ swapBuffers win
            return $ runEffect $ thePipe >-> renderPipe

        case res of
            Left  err        -> replaceMVar completion $ left err
            Right renderLoop -> do
                replaceMVar completion $ right ()
                renderLoop

    join $ lift $ takeMVar completion

    return $ 
        let pipe = do
                c <- lift $ readIORef closed
                when (not c) $ do
                    x <- await
                    lift $ replaceMVar mv x
                    pipe
        in pipe

{-| @(renderWaterfallLine width height colorMap)@ returns a Consumer that
    renders a waterfall plot into the current OpenGL context. The Consumer
    takes data that is an instance of IsPixelData and of length @width@.
    The waterfall is @height@ rows of data high.

    The fill is drawn with a vertical gradient defined by @colorMap@.

    All OpenGL based initialization of the rendering function (loading of
    shaders, etc) is performed before the pipe is returned.
-}
renderWaterfall :: IsPixelData a => Int -> Int -> [GLfloat] -> IO (Consumer a IO ())
renderWaterfall width height colorMap = do
    --Load the shaders
    vertFN <- getDataFileName "shaders/waterfall.vert"
    fragFN <- getDataFileName "shaders/waterfall.frag"
    vs <- loadShader VertexShader   vertFN
    fs <- loadShader FragmentShader fragFN
    p  <- linkShaderProgram [vs, fs]

    --Set stuff
    currentProgram $= Just p

    ab <- genObjectName 

    locc <- get $ attribLocation p "coord"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
        vad    = VertexArrayDescriptor 2 Float stride offset0

    bindBuffer ArrayBuffer  $= Just ab
    vertexAttribArray   locc $= Enabled
    vertexAttribPointer locc $= (ToFloat, vad)

    let xCoords :: [GLfloat]
        xCoords = [-1, -1, 1, -1, 1, 1, -1, 1]
    withArray xCoords $ \ptr -> 
        bufferData ArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLfloat) * 8, ptr, StaticDraw)

    let yCoords :: [GLfloat]
        yCoords = take (width * height) $ repeat 0

    activeTexture $= TextureUnit 0
    texture Texture2D $= Enabled
    to0 <- loadTexture (TexInfo (fromIntegral width) (fromIntegral height) TexMono yCoords)
    
    loc <- get $ uniformLocation p "texture"
    asUniform (0 :: GLint) loc

    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

    activeTexture $= TextureUnit 1
    texture Texture2D $= Enabled
    to1 <- loadTexture (TexInfo (fromIntegral $ length colorMap `quot` 3) 1 TexRGB colorMap)
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    loc <- get $ uniformLocation p "colorMap"
    asUniform (1 :: GLint) loc 

    let lcm :: GLfloat
        lcm = fromIntegral $ length colorMap `quot` 3
    loc <- get $ uniformLocation p "scale"
    asUniform ((lcm - 1) / lcm) loc 

    loc <- get $ uniformLocation p "offset"
    asUniform (0.5 / lcm) loc 

    loc <- get $ uniformLocation p "voffset"

    let pipe yoffset = do
            dat <- await

            lift $ do
                currentProgram $= Just p

                let textureOffset = (yoffset + height - 1) `mod` height

                withPixels dat $ \ptr -> 
                    texSubImage2D Texture2D 0 (TexturePosition2D 0 (fromIntegral textureOffset)) (TextureSize2D (fromIntegral width) 1) (PixelData Red Float ptr)

                asUniform (fromIntegral yoffset / fromIntegral height :: GLfloat) loc

                bindBuffer ArrayBuffer   $= Just ab
                vertexAttribArray   locc $= Enabled
                vertexAttribPointer locc $= (ToFloat, vad)

                activeTexture $= TextureUnit 0
                textureBinding Texture2D $= Just to0

                activeTexture $= TextureUnit 1
                textureBinding Texture2D $= Just to1

                --No idea why this is needed
                activeTexture $= TextureUnit 0

                drawArrays Quads 0 4

            pipe $ if yoffset + 1 >= height then 0 else yoffset + 1

    return $ pipe 0

