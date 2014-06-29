{-| Draw and update line graphs with OpenGL.

    Based on: <https://en.wikibooks.org/wiki/OpenGL_Programming/Scientific_OpenGL_Tutorial_02>
-}
module Graphics.DynamicGraph.TextureLine (
    graph,
    graphAsConsumer
    ) where

import Control.Monad
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array

import Pipes

import Graphics.DynamicGraph.RenderAxis

import Paths_dynamic_graph

{-| @(graph windowWidth windowHeight samples xResolution)@ creates a window
    of width @windowWidth@ and height @windowHeight@ for displaying a line
    graph. A function is returned for updating the line graph. It takes an
    instance of IsPixelData of length @samples@ as the y values and draws
    a line graph with @xResolution@ vertices. 
-}
graph :: IsPixelData a => Int -> Int -> Int -> Int -> EitherT String IO (a -> IO ())
graph width height samples xResolution = do
    res' <- lift $ createWindow width height "" Nothing Nothing
    win <- maybe (left "error creating window") return res'

    lift $ makeContextCurrent (Just win)
    mtu <- lift $ get maxVertexTextureImageUnits
    when (mtu <= 0) $ left "No texture units accessible from vertex shader"

    renderFunc <- lift $ graph' samples xResolution

    renderAxisFunc <- lift $ renderAxis width height

    return $ \dat -> do
        makeContextCurrent (Just win)

        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        renderAxisFunc

        viewport $= (Position 50 50, Size (fromIntegral width - 100) (fromIntegral height - 100))
        renderFunc dat

        swapBuffers win

graph' :: IsPixelData a => Int -> Int -> IO (a -> IO())
graph' samples xResolution = do
    --Load the shaders
    vertFN <- getDataFileName "shaders/texture_line.vert"
    fragFN <- getDataFileName "shaders/texture_line.frag"
    vs <- loadShader VertexShader   vertFN
    fs <- loadShader FragmentShader fragFN
    p  <- linkShaderProgram [vs, fs]

    --Set stuff
    currentProgram $= Just p

    ab <- genObjectName 

    loc <- get $ attribLocation p "coord"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) 
        vad    = VertexArrayDescriptor 1 Float stride offset0

    bindBuffer ArrayBuffer  $= Just ab
    vertexAttribArray   loc $= Enabled
    vertexAttribPointer loc $= (ToFloat, vad)

    let xCoords :: [GLfloat]
        xCoords = take xResolution $ iterate (+ 2 / fromIntegral xResolution) (-1)
    withArray xCoords $ \ptr -> 
        bufferData ArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLfloat) * xResolution, ptr, StaticDraw)

    let yCoords :: [GLfloat]
        yCoords = take samples $ repeat 0

    activeTexture $= TextureUnit 0
    texture Texture2D $= Enabled
    to <- loadTexture (TexInfo (fromIntegral samples) 1 TexMono yCoords)
    
    locc <- get $ uniformLocation p "texture"
    asUniform (0 :: GLint) locc

    textureFilter Texture2D     $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

    return $ \vbd -> do
        currentProgram           $= Just p
        bindBuffer ArrayBuffer   $= Just ab
        vertexAttribPointer loc  $= (ToFloat, vad)
        textureBinding Texture2D $= Just to
        reloadTexture to (TexInfo (fromIntegral samples) 1 TexMono vbd)
        drawArrays LineStrip 0 (fromIntegral xResolution)

toConsumer :: Monad m => (a -> m b) -> Consumer a m ()
toConsumer func = forever $ await >>= lift . func

-- | Same as above, but returns a Consumer instead of an update function
graphAsConsumer :: IsPixelData a => Int -> Int -> Int -> Int -> EitherT String IO (Consumer a IO ())
graphAsConsumer width height samples xResolution = liftM toConsumer $ graph width height samples xResolution

