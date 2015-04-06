{-| Render Cairo drawings with OpenGL. Useful for drawing axes.
-}

module Graphics.DynamicGraph.RenderCairo (
    renderCairo
    ) where

import Foreign.Storable
import Foreign.Marshal.Array

import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import Graphics.Rendering.Cairo hiding (height, width)

import Paths_dynamic_graph

{-| Returns a function that renders a cairo drawing into the current OpenGL context.

    All OpenGL based initialization of the rendering function (loading of shaders, rendering the cairo drawing to a texture, etc) is performed before the function is returned.

    This function must be called with an OpenGL context currently set.
-}
renderCairo :: Render a   -- ^ Cairo render monad that does the drawing
            -> Int        -- ^ X resolution
            -> Int        -- ^ Y resolution
            -> IO (IO ())
renderCairo rm width height = do

    --Render the graph to a ByteString
    dat <- withImageSurface FormatARGB32 width height $ \surface -> do
        renderWith surface rm 
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

