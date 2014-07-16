module Graphics.DynamicGraph.Util (
    setupGLFW,
    replaceMVar
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Concurrent.MVar
import Control.DeepSeq

import Graphics.UI.GLFW as G

-- | Utility function to setup GLFW for graph drawing
setupGLFW :: EitherT String IO ()
setupGLFW = do
    lift $ setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg

    res <- lift $ G.init
    unless res (left "error initializing glfw")

replaceMVar :: MVar a -> a -> IO ()
replaceMVar mv val = do
    tryTakeMVar mv
    putMVar mv val

