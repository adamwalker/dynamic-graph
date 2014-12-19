{-# LANGUAGE ScopedTypeVariables #-}

{-| Draw and update filled in line graphs with OpenGL.
-}

module Graphics.DynamicGraph.FillLine (
    filledLineWindow,
    renderFilledLine
    ) where

import Control.Monad
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef

import Pipes

import Graphics.DynamicGraph.Util

import Paths_dynamic_graph

{-| @(filledLineWindow windowWidth windowHeight samples colorMap)@ creates
    a window of width @windowWidth@ and height @windowHeight@ for
    displaying a line graph. 
    
    A function is returned for dynamically updating the line graph. It
    takes an instance of IsPixelData of length @samples@ as the y values. 
    
    The fill is drawn with a vertical gradient defined by @colorMap@.
-}
filledLineWindow :: IsPixelData a => Int -> Int -> Int -> [GLfloat] -> EitherT String IO (Consumer a IO ())
filledLineWindow width height samples colorMap = do
    mv :: MVar a <- lift $ newEmptyMVar
    completion <- lift $ newEmptyMVar

    closed <- lift $ newIORef False

    lift $ forkOS $ void $ do
        res <- runEitherT $ do
            res' <- lift $ createWindow width height "" Nothing Nothing
            win <- maybe (left "error creating window") return res'
            lift $ setWindowSizeCallback win $ Just $ \win x y -> do
                viewport $= (Position 0 0, Size (fromIntegral x) (fromIntegral y))
            lift $ setWindowCloseCallback win $ Just $ \win -> writeIORef closed True
            lift $ makeContextCurrent (Just win)
            lift $ clearColor $= Color4 0 0 0 0

            renderFunc <- lift $ renderFilledLine samples colorMap

            return $ forever $ do
                pollEvents
                dat <- takeMVar mv
                makeContextCurrent (Just win)
                clear [ColorBuffer]
                renderFunc dat
                swapBuffers win

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

{-| @(renderFilledLine samples colorMap)@ returns a function that
    renders a filled in line graph into the current OpenGL context. The
    function takes an instance of IsPixelData of length @samples@.

    The fill is drawn with a vertical gradient defined by @colorMap@.

    All OpenGL based initialization of the rendering function (loading of
    shaders, etc) is performed before the function is returned.
-}
renderFilledLine :: IsPixelData a => Int -> [GLfloat] -> IO (a -> IO ())
renderFilledLine samples colorMap = do
    --Load the shaders
    vertFN <- getDataFileName "shaders/fill_line.vert"
    fragFN <- getDataFileName "shaders/fill_line.frag"
    vs <- loadShader VertexShader   vertFN
    fs <- loadShader FragmentShader fragFN
    p  <- linkShaderProgram [vs, fs]

    --Set stuff
    currentProgram $= Just p

    ab <- genObjectName 
    locc <- get $ attribLocation p "coord"

    --The quad that covers the whole screen
    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
        vad    = VertexArrayDescriptor 2 Float stride offset0

    bindBuffer ArrayBuffer  $= Just ab
    vertexAttribArray   locc $= Enabled
    vertexAttribPointer locc $= (ToFloat, vad)

    let xCoords :: [GLfloat]
        xCoords = [-1, -1, 1, -1, 1, 1, -1, 1]
    withArray xCoords $ \ptr -> 
        bufferData ArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLfloat) * 8, ptr, StaticDraw)

    --The y coordinates
    let yCoords :: [GLfloat]
        yCoords = take samples $ repeat 0

    activeTexture $= TextureUnit 0
    texture Texture2D $= Enabled
    to <- loadTexture (TexInfo (fromIntegral samples) 1 TexMono yCoords)

    loc <- get $ uniformLocation p "texture"
    asUniform (0 :: GLint) loc 
    
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

    --The color map
    activeTexture $= TextureUnit 1
    texture Texture2D $= Enabled
    loadTexture (TexInfo (fromIntegral $ length colorMap `quot` 3) 1 TexRGB colorMap)
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
    
    --No idea why this is needed
    activeTexture $= TextureUnit 0

    return $ \vbd -> do
        currentProgram $= Just p
        reloadTexture to (TexInfo (fromIntegral samples) 1 TexMono vbd)

        bindBuffer ArrayBuffer  $= Just ab
        vertexAttribArray   locc $= Enabled
        vertexAttribPointer locc $= (ToFloat, vad)

        drawArrays Quads 0 4

