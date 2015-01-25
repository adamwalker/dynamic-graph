{-# LANGUAGE ScopedTypeVariables #-}

{-| Draw and update line graphs with OpenGL.

    Based on: <https://en.wikibooks.org/wiki/OpenGL_Programming/Scientific_OpenGL_Tutorial_02>

    Example usage:

> import Control.Monad
> import Control.Monad.Trans.Either
> import Control.Concurrent
> import Pipes
> import qualified Pipes.Prelude as P
> import System.Random
> import Graphics.Rendering.OpenGL
> 
> import Graphics.DynamicGraph.Line
> 
> randomVect :: Producer [GLfloat] IO ()
> randomVect =  P.repeatM $ do
>     res <- replicateM 1000 randomIO
>     threadDelay 10000
>     return res
> 
> main = eitherT putStrLn return $ do
>     setupGLFW
>     lineGraph <- lineWindow 1024 480 1000 1024 
> 
>     lift $ runEffect $ randomVect >-> lineGraph
-}

module Graphics.DynamicGraph.Line (
    renderLine,
    setupGLFW
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

import Graphics.DynamicGraph.RenderCairo
import Graphics.DynamicGraph.Axis
import Graphics.DynamicGraph.Util

import Paths_dynamic_graph

{-| @(renderLine samples xResolution)@ returns a function that
    renders a line graph into the current OpenGL context. The function
    takes an instance of IsPixelData of length @samples@ and draws a line
    graph with @xResolution@ vertices. 

    All OpenGL based initialization of the rendering function (loading of
    shaders, etc) is performed before the function is returned.
-}
renderLine :: IsPixelData a => Int -> Int -> IO (a -> IO())
renderLine samples xResolution = do
    --Load the shaders
    vertFN <- getDataFileName "shaders/line.vert"
    fragFN <- getDataFileName "shaders/line.frag"
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

