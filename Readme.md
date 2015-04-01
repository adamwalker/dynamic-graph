# Dynamic-Graph

Draw and update graphs in real time with OpenGL. Suitable for displaying large amounts of frequently changing data. Line graphs and waterfall plots are supported, as well as axis drawing.

# Screenshots

![Screenshot](../screenshots/screenshots/line.png?raw=true)
![Screenshot](../screenshots/screenshots/waterfall.png?raw=true)

# Installation

dynamic-graph is available on [Hackage](https://hackage.haskell.org/package/dynamic-graph). Install with `cabal install dynamic-graph`.

# Usage

To plot a waterfall of random data:

```haskell

import Control.Monad
import Control.Monad.Trans.Either
import Control.Concurrent
import Pipes
import qualified Pipes.Prelude as P
import System.Random
import Graphics.Rendering.OpenGL

import Graphics.DynamicGraph.Waterfall
import Graphics.DynamicGraph.Util

randomVect :: Producer [GLfloat] IO ()
randomVect =  P.repeatM $ do
    res <- replicateM 1000 randomIO
    threadDelay 10000
    return res

main = eitherT putStrLn return $ do
    setupGLFW
    waterfall <- waterfallWindow 1024 480 1000 1000 jet_mod

    lift $ runEffect $ randomVect >-> waterfall

```
