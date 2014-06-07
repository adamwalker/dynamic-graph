module DynamicGraph.Waterfall where

import Control.Monad
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array

import Pipes

import Paths_dynamic_graph

graph :: IsPixelData a => Int -> Int -> EitherT String IO (Consumer a IO ())
graph width height = do
    res' <- lift $ createWindow 1024 768 "" Nothing Nothing
    win <- maybe (left "error creating window") return res'

    lift $ do
        makeContextCurrent (Just win)

        mtu <- get maxVertexTextureImageUnits
        print mtu

        --Load the shaders
        vertFN <- getDataFileName "shaders/waterfall.vert"
        fragFN <- getDataFileName "shaders/waterfall.frag"
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

        let xCoords :: [GLfloat]
            xCoords = [-1, -1, 1, -1, 1, 1, -1, 1]
        withArray xCoords $ \ptr -> 
            bufferData ArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLfloat) * 8, ptr, StaticDraw)

        let yCoords :: [GLfloat]
            yCoords = take (width * height) $ repeat 0

        --texture Texture2D $= Enabled
        to <- loadTexture (TexInfo (fromIntegral width) (fromIntegral height) TexMono yCoords)
        
        --activeTexture $= TextureUnit 0
        --loc <- get $ uniformLocation p "texture"
        --asUniform (0 :: GLint) loc 

        textureFilter Texture2D $= ((Nearest, Nothing), Nearest)

        textureWrapMode Texture2D S $= (Repeated, Repeat)
        textureWrapMode Texture2D T $= (Repeated, Repeat)

        loc <- get $ uniformLocation p "voffset"

        let pipe yoffset = do
                dat <- await

                lift $ do
                    makeContextCurrent (Just win)
                    clear [ColorBuffer, DepthBuffer]

                    let textureOffset = (yoffset + height - 1) `mod` height

                    withPixels dat $ \ptr -> texSubImage2D Texture2D 0 (TexturePosition2D 0 (fromIntegral textureOffset)) (TextureSize2D (fromIntegral width) 1) (PixelData Red Float ptr)

                    asUniform (fromIntegral yoffset / fromIntegral height :: GLfloat) loc

                    drawArrays Quads 0 4
                    swapBuffers win

                pipe $ if yoffset + 1 >= height then 0 else yoffset + 1

        return $ pipe 0
