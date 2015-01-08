module Graphics.DynamicGraph.ColorMaps (
    jet,
    jet_mod,
    hot,
    bw,
    wb,
    ) where

import Graphics.Rendering.OpenGL

-- | The matlab / octave \"jet\" color map
jet :: [GLfloat]
jet =  [0, 0, 0.5,  0, 0, 1,  0, 0.5, 1,   0, 1, 1,  0.5, 1, 0.5,  1, 1, 0,  1, 0.5, 0,  1, 0, 0,  0.5, 0, 0]

-- | \"jet\" modified so that low values are a darker blue
jet_mod :: [GLfloat]
jet_mod =  [0, 0, 0.1,  0, 0, 1,  0, 0.5, 1,   0, 1, 1,  0.5, 1, 0.5,  1, 1, 0,  1, 0.5, 0,  1, 0, 0,  0.5, 0, 0]

-- | The matlab / octave \"hot\" color map
hot :: [GLfloat]
hot =  [0, 0, 0,  1, 0, 0,  1, 1, 0,  1, 1, 1]

-- | Ranges from black to white
bw :: [GLfloat]
bw =  [0, 0, 0, 1, 1, 1]

-- | Ranges from white to black
wb :: [GLfloat]
wb =  [1, 1, 1, 0, 0, 0]

