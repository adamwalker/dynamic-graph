module DynamicGraph.SimpleLine where

import Control.Monad
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array

import Paths_dynamic_graph

graph :: Storable a => EitherT String IO ([a] -> IO ())
graph = do
    res' <- lift $ createWindow 1024 768 "" Nothing Nothing
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

        return $ \vbd -> do
            makeContextCurrent (Just win)
            clear [ColorBuffer]
            withArray vbd $ \ptr -> 
                bufferData ArrayBuffer $= (fromIntegral $ length vbd * sizeOf (head vbd), ptr, StaticDraw)
            drawArrays LineStrip 0 (fromIntegral $ length vbd `quot` 2)
            swapBuffers win

