{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.DynamicGraph.ThreeD (
    renderThreeD,
    ) where

import Control.Monad
import Control.Concurrent hiding (yield)
import Control.Concurrent.MVar
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Linear

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array
import Data.IORef
import Foreign.Ptr

import Pipes

import Paths_dynamic_graph

{-| @(renderThreeD width height colorMap)@ returns a Consumer that
    renders a waterfall plot into the current OpenGL context. The Consumer
    takes data that is an instance of IsPixelData and of length @width@.
    The waterfall is @height@ rows of data high.

    The fill is drawn with a vertical gradient defined by @colorMap@.

    All OpenGL based initialization of the rendering function (loading of
    shaders, etc) is performed before the pipe is returned.
-}
renderThreeD :: (IsPixelData a, Show a) => Int -> Int -> Int -> Int -> IO (Consumer a IO ())
renderThreeD width height numPointsX numPointsY = do
    --Load the shaders
    vertFN <- getDataFileName "shaders/threeD.vert"
    fragFN <- getDataFileName "shaders/threeD.frag"
    vs <- loadShader VertexShader   vertFN
    fs <- loadShader FragmentShader fragFN
    p  <- linkShaderProgram [vs, fs]

    --Set stuff
    currentProgram $= Just p

    let numPointsXDiv2 = (fromIntegral numPointsX - 1) / 2
        numPointsYDiv2 = (fromIntegral numPointsY - 1) / 2

    --Create the index buffer object. This holds the indices of the vertices in the order that we will draw them.
    let horiz = do
            y <- [0 .. numPointsY - 1]
            x <- [0 .. numPointsX - 2]
            [y * numPointsX + x, y * numPointsX + x + 1]

    let vert = do
            x <- [0 .. numPointsX - 1]
            y <- [0 .. numPointsY - 2]
            [y * numPointsX + x, (y + 1) * numPointsX + x]

    let indices :: [GLushort] = map fromIntegral $ horiz ++ vert

    ibo <- genObjectName 
    bindBuffer ElementArrayBuffer $= Just ibo
    withArray indices $ \ptr -> 
        bufferData ElementArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLushort) * length indices, ptr, StaticDraw)

    --Create the array buffer object that contains the 2D vertices.
    let vertices :: [GLfloat] = do
            i <- [0 .. numPointsY - 1]
            j <- [0 .. numPointsX - 1]
            [(fromIntegral i - numPointsYDiv2) / numPointsYDiv2, (fromIntegral j - numPointsXDiv2) / numPointsXDiv2]

    vbo <- genObjectName 
    bindBuffer ArrayBuffer $= Just vbo
    locc <- get $ attribLocation p "coord2d"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
        vad    = VertexArrayDescriptor 2 Float stride offset0
        
    vertexAttribArray   locc $= Enabled
    vertexAttribPointer locc $= (ToFloat, vad)

    withArray vertices $ \ptr -> 
        bufferData ArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLfloat) * length vertices, ptr, StaticDraw)

    --Create a 2D texture. This holds the graph data.
    let zCoords :: [GLfloat]
        zCoords = take (width * height) $ repeat 0
    activeTexture $= TextureUnit 0
    texture Texture2D $= Enabled
    to0 <- loadTexture (TexInfo (fromIntegral width) (fromIntegral height) TexMono zCoords)
    loc <- get $ uniformLocation p "texture"
    asUniform (0 :: GLint) loc

    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

    --loc <- get $ uniformLocation p "voffset"
    let pm = projectionMatrix (deg2rad 30 :: GLfloat) (1024 / 768) 0.1 10
        --c  = camMatrix $ dolly (V3 0 0 (6 :: GLfloat)) fpsCamera
        c  = Linear.lookAt (V3 3 3 5) (V3 0 0 0) (V3 0 0 1)
    mat <- get $ uniformLocation p "vertex_transform"
    asUniform (pm !*! c) mat

    loc <- get $ uniformLocation p "voffset"

    let pipe yoffset = do
            dat <- await

            lift $ do
                currentProgram $= Just p

                let textureOffset = (yoffset + 1) `mod` height

                withPixels dat $ \ptr -> 
                    texSubImage2D Texture2D 0 (TexturePosition2D 0 (fromIntegral textureOffset)) (TextureSize2D (fromIntegral width) 1) (PixelData Red Float ptr)

                asUniform (fromIntegral textureOffset / fromIntegral height :: GLfloat) loc

                activeTexture $= TextureUnit 0
                textureBinding Texture2D $= Just to0

                --because the opengl tutorial does
                vertexAttribArray   locc $= Enabled
                bindBuffer ArrayBuffer $= Just vbo
                vertexAttribPointer locc $= (ToFloat, vad)

                bindBuffer ElementArrayBuffer $= Just ibo
                drawElements Lines (fromIntegral $ length indices) UnsignedShort nullPtr

            pipe $ if yoffset + 1 >= height then 0 else yoffset + 1

    return $ pipe 0

