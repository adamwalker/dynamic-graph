{-| Various utility functions -}
module Graphics.DynamicGraph.Util (
    setupGLFW,
    checkVertexTextureUnits,
    replaceMVar,
    pipeify
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Concurrent.MVar
import Control.Applicative

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as G
import Pipes

-- | Utility function to setup GLFW for graph drawing. Returns True on success.
setupGLFW :: IO Bool
setupGLFW = do
    setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg
    G.init
    
-- | Returns True if texture units are accessible from the vertex shader. Needed  by Graphics.DynamicGraph.Line.
checkVertexTextureUnits :: IO Bool
checkVertexTextureUnits = (> 0) <$> get maxVertexTextureImageUnits

-- | `tryTakeMVar` then `putMVar`
replaceMVar :: MVar a -> a -> IO ()
replaceMVar mv val = do
    tryTakeMVar mv
    putMVar mv val

-- | Convert a function that performs a monadic action to a Consumer
pipeify :: Monad m => (a -> m ()) -> Consumer a m ()
pipeify = for cat . (lift .)
