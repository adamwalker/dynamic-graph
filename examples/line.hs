import Control.Monad
import Control.Monad.Trans.Either
import Control.Concurrent
import Control.Applicative
import Pipes
import qualified Pipes.Prelude as P
import System.Random
import Graphics.Rendering.OpenGL

import Graphics.DynamicGraph.Line
import Graphics.DynamicGraph.Window

randomVect :: Producer [GLfloat] IO ()
randomVect =  P.repeatM $ do
    res <- replicateM 1000 randomIO
    threadDelay 10000
    return res

main = eitherT putStrLn return $ do
    res <- lift setupGLFW
    unless res (left "Unable to initilize GLFW")

    lineGraph <- window 1024 480 $ pipeify <$> renderLine 1000 1024

    lift $ runEffect $ randomVect >-> lineGraph
