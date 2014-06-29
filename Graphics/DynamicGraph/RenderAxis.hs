module Graphics.DynamicGraph.RenderAxis where

import Control.Monad
import Graphics.UI.GLFW as G
import Graphics.Rendering.OpenGL
import Graphics.GLUtil

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Foreign.Storable
import Foreign.Marshal.Array

import Data.Colour.RGBSpace
import Data.Colour.SRGB
import Data.Colour.Names
import Graphics.Rendering.Cairo hiding (height, width)
import Graphics.Rendering.Pango

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Graphics.DynamicGraph.Axis

import Paths_dynamic_graph

renderAxis :: Int -> Int -> IO (IO ())
renderAxis width height = do

    --Render the graph to a ByteString
    dat <- withImageSurface FormatARGB32 width height $ \surface -> do
        (renderAxes (defaultConfiguration {width = fromIntegral width, height = fromIntegral height}) surface) 
        imageSurfaceGetData surface

    --Load the shaders
    vertFN <- getDataFileName "shaders/cairo.vert"
    fragFN <- getDataFileName "shaders/cairo.frag"
    vs <- loadShader VertexShader   vertFN
    fs <- loadShader FragmentShader fragFN
    p  <- linkShaderProgram [vs, fs]

    --Set stuff
    currentProgram $= Just p
    ab <- genObjectName 

    locc <- get $ attribLocation p "coord"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
        vad    = VertexArrayDescriptor 2 Float stride offset0

    bindBuffer ArrayBuffer   $= Just ab
    vertexAttribArray   locc $= Enabled
    vertexAttribPointer locc $= (ToFloat, vad)

    let xCoords :: [GLfloat]
        xCoords = [-1, -1, 1, -1, 1, 1, -1, 1]
    withArray xCoords $ \ptr -> 
        bufferData ArrayBuffer $= (fromIntegral $ sizeOf(undefined::GLfloat) * 8, ptr, StaticDraw)

    activeTexture $= TextureUnit 0
    texture Texture2D $= Enabled
    to <- genObjectName 
    textureBinding Texture2D $= Just to
    withPixels dat $ texImage2D Texture2D NoProxy 0 RGBA8 (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 . PixelData BGRA UnsignedInt8888Rev
    
    loc <- get $ uniformLocation p "texture"
    asUniform (0 :: GLint) loc 

    textureFilter Texture2D     $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

    return $ do
        currentProgram           $= Just p
        bindBuffer ArrayBuffer   $= Just ab
        vertexAttribPointer locc $= (ToFloat, vad)
        textureBinding Texture2D $= Just to
        drawArrays Quads 0 4

