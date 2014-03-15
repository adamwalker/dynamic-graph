module DynamicGraph.SimpleLine where

import Control.Monad
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array

import Paths_dynamic_graph

graph :: Storable a => EitherT String IO ([a] -> IO ())
graph = do
    lift $ setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg

    res <- lift $ G.init
    unless res (left "error initializing glfw")

    res' <- lift $ createWindow 1024 768 "" Nothing Nothing
    win <- maybe (left "error creating window") return res'

    lift $ do
        makeContextCurrent (Just win)

        --Load the shaders
        vertFN <- getDataFileName "DynamicGraph/shader.vert"
        fragFN <- getDataFileName "DynamicGraph/shader.frag"
        vs <- loadShader VertexShader   vertFN
        fs <- loadShader FragmentShader fragFN
        p  <- linkShaderProgram [vs, fs]

        --Set stuff
        clearColor $= Color4 1 1 1 1
        currentProgram $= Just p

        [ab] <- genObjectNames 1

        loc <- get $ attribLocation p "coord"

        let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
            vad    = VertexArrayDescriptor 2 Float stride offset0

        bindBuffer ArrayBuffer  $= Just ab
        vertexAttribArray   loc $= Enabled
        vertexAttribPointer loc $= (ToFloat, vad)

        return $ \vbd -> do
            clear [ColorBuffer, DepthBuffer]
            withArray vbd $ \ptr -> 
                bufferData ArrayBuffer $= (fromIntegral $ length vbd * sizeOf (head vbd), ptr, StaticDraw)
            drawArrays LineStrip 0 (fromIntegral $ length vbd `quot` 2)
            swapBuffers win

