module DynamicGraph.TextureLine where

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

graph :: IsPixelData a => Int -> EitherT String IO (a -> IO ())
graph samples = do
    res' <- lift $ createWindow 1024 768 "" Nothing Nothing
    win <- maybe (left "error creating window") return res'

    lift $ do
        makeContextCurrent (Just win)

        mtu <- get maxVertexTextureImageUnits
        print mtu

        --Load the shaders
        vertFN <- getDataFileName "shaders/texture_line.vert"
        fragFN <- getDataFileName "shaders/texture_line.frag"
        vs <- loadShader VertexShader   vertFN
        fs <- loadShader FragmentShader fragFN
        p  <- linkShaderProgram [vs, fs]

        --Set stuff
        clearColor $= Color4 1 1 1 1
        currentProgram $= Just p

        ab <- genObjectName 

        loc <- get $ attribLocation p "coord"

        let stride = fromIntegral $ sizeOf (undefined::GLfloat) 
            vad    = VertexArrayDescriptor 1 Float stride offset0

        bindBuffer ArrayBuffer  $= Just ab
        vertexAttribArray   loc $= Enabled
        vertexAttribPointer loc $= (ToFloat, vad)

        let xCoords :: [GLfloat]
            xCoords = take samples $ iterate (+ 2 / fromIntegral samples) (-1)
        withArray xCoords $ \ptr -> 
            bufferData ArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLfloat) * samples, ptr, StaticDraw)

        let yCoords :: [GLfloat]
            yCoords = take samples $ repeat 0

        --texture Texture2D $= Enabled
        to <- loadTexture (TexInfo (fromIntegral samples) 1 TexMono yCoords)
        
        --activeTexture $= TextureUnit 0
        --loc <- get $ uniformLocation p "texture"
        --asUniform (0 :: GLint) loc 

        textureFilter Texture2D $= ((Linear', Nothing), Linear')

        textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
        textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

        return $ \vbd -> do
            makeContextCurrent (Just win)
            clear [ColorBuffer, DepthBuffer]

            reloadTexture to (TexInfo (fromIntegral samples) 1 TexMono vbd)

            drawArrays LineStrip 0 (fromIntegral samples)
            swapBuffers win

toConsumer :: Monad m => (a -> m b) -> Consumer a m ()
toConsumer func = forever $ await >>= lift . func

graphAsComsumer :: IsPixelData a => Int -> EitherT String IO (Consumer a IO ())
graphAsComsumer = liftM toConsumer . graph

