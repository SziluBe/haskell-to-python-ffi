module RandEntityGenerator where

import System.Random ()
import Graphics.Gloss ( circle )
import Codec.Picture ()
import Graphics.Gloss.Interface.Pure.Game ()

import LinAlg ( V2(V2, x) )
import Entity
    ( Entity(entityID, location, size, velocity, sprite), baseEntity )
import GameState
    ( GameState(dimensions, highestEntityId),
      fALLING_CIRCLE_RADIUS,
      getRandom )

genRandX :: GameState -> (Float, GameState)
genRandX s = getRandom (0, x $ dimensions s) s

-- entity, ID, state, does not add the created Entity to the GameState
genEntityAtRandX :: GameState -> Float -> (Entity, Int, GameState)
genEntityAtRandX s y = (e, newMaxEntityID, s1)
    where
        (randX, s1)    = genRandX s
        newMaxEntityID = highestEntityId s + 1
        e              = baseEntity { entityID    = newMaxEntityID
                                    , location    = V2 randX y }

-- state -> y-coord. -> (entity, ID, state); does not add the created Entity to the GameState
genFallingCircle :: GameState -> Float -> (Entity, Int, GameState)
genFallingCircle s y = (e, newMaxEntityID, s2)
    where
        (e1, newMaxEntityID, s1) = genEntityAtRandX s y
        (v, s2) = genRandVelocity s1
        e       = e1 { size        = V2 (2 * fALLING_CIRCLE_RADIUS) (2 * fALLING_CIRCLE_RADIUS)
                     , velocity    = v
                     , sprite      = circle fALLING_CIRCLE_RADIUS }

genRandVelocity :: GameState -> (V2, GameState)
genRandVelocity s = (V2 vX vY, s2)
    where
        (vX, s1) = getRandom (-5, 5) s
        (vY, s2) = getRandom (-5, -10) s1
