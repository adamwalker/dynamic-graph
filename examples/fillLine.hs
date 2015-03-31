import Control.Monad
import Control.Monad.Trans.Either
import Control.Concurrent
import Pipes
import qualified Pipes.Prelude as P
import System.Random
import Graphics.Rendering.OpenGL

import Graphics.DynamicGraph.FillLine
import Graphics.DynamicGraph.Window

randomVect :: Producer [GLfloat] IO ()
randomVect =  P.repeatM $ do
    res <- replicateM 1000 randomIO
    threadDelay 10000
    return res

main = eitherT putStrLn return $ do
    setupGLFW
    lineGraph  <- window 1024 480 $ fmap (for cat . (lift . )) $ renderFilledLine 1000 jet_mod

    lift $ runEffect $ randomVect >-> lineGraph
