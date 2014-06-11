module DynamicGraph.Util where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either

import Graphics.UI.GLFW as G

setupGLFW :: EitherT String IO ()
setupGLFW = do
    lift $ setErrorCallback $ Just $ \error msg -> do
        print error
        putStrLn msg

    res <- lift $ G.init
    unless res (left "error initializing glfw")
