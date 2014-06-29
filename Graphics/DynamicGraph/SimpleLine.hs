{-| Draw and update line graphs with OpenGL.

    Based on: <https://en.wikibooks.org/wiki/OpenGL_Programming/Scientific_OpenGL_Tutorial_01>

    You probably want to use "Graphics.DynamicGraph.TextureLine" as it is better.
-}
module Graphics.DynamicGraph.SimpleLine (
    simpleLineWindow
    ) where

import Control.Monad
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Ptr

import Paths_dynamic_graph

{-| @(simpleLineWindow windowWidth windowHeight bufLen)@ creates a window
    of width @windowWidth@ and height @windowHeight@ for displaying a line
    graph. A function is returned for updating the line graph. It takes
    a pointer to a c array of length @bufLen@ consisting of pairs of \<x,
    y\> coordinates for updating the graph as this is the format that
    OpenGL requires.
-}
simpleLineWindow :: Int -> Int -> Int -> EitherT String IO (Ptr GLfloat -> IO ())
simpleLineWindow width height bufLen = do
    res' <- lift $ createWindow width height "" Nothing Nothing
    win <- maybe (left "error creating window") return res'

    lift $ do
        makeContextCurrent (Just win)

        --Load the shaders
        vertFN <- getDataFileName "shaders/simple_line.vert"
        fragFN <- getDataFileName "shaders/simple_line.frag"
        vs <- loadShader VertexShader   vertFN
        fs <- loadShader FragmentShader fragFN
        p  <- linkShaderProgram [vs, fs]

        --Set stuff
        clearColor $= Color4 1 1 1 1
        currentProgram $= Just p

        ab <- genObjectName

        loc <- get $ attribLocation p "coord"

        let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
            vad    = VertexArrayDescriptor 2 Float stride offset0

        bindBuffer ArrayBuffer  $= Just ab
        vertexAttribArray   loc $= Enabled
        vertexAttribPointer loc $= (ToFloat, vad)

        return $ \ptr -> do
            makeContextCurrent (Just win)
            clear [ColorBuffer]
            bufferData ArrayBuffer $= (fromIntegral $ 2 * bufLen * sizeOf (undefined :: GLfloat), ptr, StaticDraw)
            drawArrays LineStrip 0 (fromIntegral bufLen)
            swapBuffers win

