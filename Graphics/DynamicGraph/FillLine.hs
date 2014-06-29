{-| Draw and update line graphs with OpenGL.

    Based on: <https://en.wikibooks.org/wiki/OpenGL_Programming/Scientific_OpenGL_Tutorial_02>
-}
module Graphics.DynamicGraph.FillLine (
    graph,
    graph'
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

import Paths_dynamic_graph

{-| @(graph windowWidth windowHeight samples xResolution)@ creates a window
    of width @windowWidth@ and height @windowHeight@ for displaying a line
    graph. A function is returned for updating the line graph. It takes an
    instance of IsPixelData of length @samples@ as the y values and draws
    a line graph with @xResolution@ vertices. 
-}
graph :: IsPixelData a => Int -> Int -> Int -> [GLfloat] -> EitherT String IO (a -> IO ())
graph width height samples colorMap = do
    res' <- lift $ createWindow width height "" Nothing Nothing
    win <- maybe (left "error creating window") return res'

    lift $ makeContextCurrent (Just win)

    renderFunc <- lift $ graph' samples colorMap

    lift $ clearColor $= Color4 0 0 0 0

    return $ \dat -> do
        makeContextCurrent (Just win)
        clear [ColorBuffer]
        renderFunc dat
        swapBuffers win

graph' :: IsPixelData a => Int -> [GLfloat] -> IO (a -> IO ())
graph' samples colorMap = do
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

