import Control.Monad
import Control.Monad.Trans.Either
import Control.Concurrent
import Pipes
import qualified Pipes.Prelude as P
import System.Random
import Graphics.Rendering.OpenGL

import Graphics.DynamicGraph.ThreeD
import Graphics.DynamicGraph.Window

randomVect :: Producer [GLfloat] IO ()
randomVect =  P.repeatM $ do
    res <- replicateM 1000 randomIO
    threadDelay 1000000
    return res

main = eitherT putStrLn return $ do
    setupGLFW
    waterfall <- window 1024 480 $ renderThreeD 50 50 50 50

    lift $ runEffect $ randomVect >-> waterfall
