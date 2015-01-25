{-# LANGUAGE ScopedTypeVariables #-}

{-| Draw and update filled in line graphs with OpenGL.

Example usage:

> import Control.Monad
> import Control.Monad.Trans.Either
> import Control.Concurrent
> import Pipes
> import qualified Pipes.Prelude as P
> import System.Random
> import Graphics.Rendering.OpenGL
> 
> import Graphics.DynamicGraph.FillLine
> 
> randomVect :: Producer [GLfloat] IO ()
> randomVect =  P.repeatM $ do
>     res <- replicateM 1000 randomIO
>     threadDelay 10000
>     return res
> 
> main = eitherT putStrLn return $ do
>     setupGLFW
>     lineGraph <- filledLineWindow 1024 480 1000 jet_mod
> 
>     lift $ runEffect $ randomVect >-> lineGraph
-}

module Graphics.DynamicGraph.FillLine (
    renderFilledLine,
    setupGLFW,
    module Graphics.DynamicGraph.ColorMaps
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
import Graphics.DynamicGraph.ColorMaps

import Paths_dynamic_graph

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

