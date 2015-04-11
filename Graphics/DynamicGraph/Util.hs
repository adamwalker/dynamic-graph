{-| Various utility functions -}
module Graphics.DynamicGraph.Util (
    setupGLFW,
    replaceMVar,
    checkVertexTextureUnits
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Concurrent.MVar

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as G

-- | Utility function to setup GLFW for graph drawing
setupGLFW :: EitherT String IO ()
setupGLFW = do
    lift $ setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg

    res <- lift $ G.init
    unless res (left "error initializing glfw")

-- | `tryTakeMVar` then `putMVar`
replaceMVar :: MVar a -> a -> IO ()
replaceMVar mv val = do
    tryTakeMVar mv
    putMVar mv val

-- | Check if texture units are accessible from the vertex shader. Needed  by Graphics.DynamicGraph.Line.
checkVertexTextureUnits :: EitherT String IO ()
checkVertexTextureUnits = do
    mtu <- lift $ get maxVertexTextureImageUnits
    when (mtu <= 0) $ left "No texture units accessible from vertex shader"

