{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.DynamicGraph.Window (
    window
    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef
import Control.Monad

import Graphics.UI.GLFW as G
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Pipes

import Graphics.DynamicGraph.Util

window :: IsPixelData a => Int -> Int -> (IO (a -> IO ())) -> EitherT String IO (Consumer a IO ())
window width height renderFunc = do
    mv :: MVar a <- lift $ newEmptyMVar
    completion <- lift $ newEmptyMVar

    closed <- lift $ newIORef False

    lift $ forkOS $ void $ do
        res <- runEitherT $ do
            res' <- lift $ createWindow width height "" Nothing Nothing
            win <- maybe (left "error creating window") return res'
            lift $ setWindowSizeCallback win $ Just $ \win x y -> do
                viewport $= (Position 0 0, Size (fromIntegral x) (fromIntegral y))
            lift $ setWindowCloseCallback win $ Just $ \win -> writeIORef closed True
            lift $ makeContextCurrent (Just win)
            lift $ clearColor $= Color4 0 0 0 0

            renderFunc <- lift renderFunc

            return $ forever $ do
                pollEvents
                dat <- takeMVar mv
                makeContextCurrent (Just win)
                clear [ColorBuffer]
                renderFunc dat
                swapBuffers win

        case res of
            Left  err        -> replaceMVar completion $ left err
            Right renderLoop -> do
                replaceMVar completion $ right ()
                renderLoop

    join $ lift $ takeMVar completion

    return $ 
        let pipe = do
                c <- lift $ readIORef closed
                when (not c) $ do
                    x <- await
                    lift $ replaceMVar mv x
                    pipe
        in pipe

