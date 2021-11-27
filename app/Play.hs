module Play where

import Graphics.Gloss ()
import Graphics.Gloss.Interface.Pure.Game
    ( Key(SpecialKey),
      KeyState(Up, Down),
      SpecialKey(KeyLeft, KeyUp, KeyDown, KeyRight),
      Event(EventKey) )

import LinAlg ( V2(V2, x, y) )
import Entity ( Entity(velocity, size, entityClass, location) )
import GameState
    ( GameState(particles, time, entities, hits, prevGenTime),
      xSize,
      ySize,
      fALLING_CIRCLE_RADIUS,
      addEntity,
      getEntitiesByClass,
      removeEntitiesByCondition,
      changeEntity )
import RandEntityGenerator ( genFallingCircle )

-- eventFromBasics :: 

inputHandler :: Event -> GameState -> GameState
inputHandler event@(EventKey key keyState mods pos) state
      | SpecialKey KeyUp            <- key
      , Down                        <- keyState
      = changeEntity movedUpPlayer state

      | SpecialKey KeyUp            <- key
      , Up                          <- keyState
      = changeEntity notMovedUpPlayer state

      | SpecialKey KeyDown          <- key
      , Down                        <- keyState
      = changeEntity movedDownPlayer state

      | SpecialKey KeyDown          <- key
      , Up                          <- keyState
      = changeEntity notMovedDownPlayer state

      | SpecialKey KeyRight         <- key
      , Down                        <- keyState
      = changeEntity movedRightPlayer state

      | SpecialKey KeyRight         <- key
      , Up                          <- keyState
      = changeEntity notMovedRightPlayer state

      | SpecialKey KeyLeft          <- key
      , Down                        <- keyState
      = changeEntity movedLeftPlayer state

      | SpecialKey KeyLeft          <- key
      , Up                          <- keyState
      = changeEntity notMovedLeftPlayer state

      where
          player           = head $ getEntitiesByClass state "Player"
        --   stationaryPlayer = player { velocity = V2 0 0 }
          movedUpPlayer       = player { velocity = V2 (x $ velocity player)     (y $ velocity player + 5) }
          movedDownPlayer     = player { velocity = V2 (x $ velocity player)     (y $ velocity player - 5) }
          movedRightPlayer    = player { velocity = V2 (x $ velocity player + 5) (y $ velocity player) }
          movedLeftPlayer     = player { velocity = V2 (x $ velocity player - 5) (y $ velocity player) }
          notMovedUpPlayer    = player { velocity = V2 (x $ velocity player)     (y $ velocity player - 5) }
          notMovedDownPlayer  = player { velocity = V2 (x $ velocity player)     (y $ velocity player + 5) }
          notMovedRightPlayer = player { velocity = V2 (x $ velocity player - 5) (y $ velocity player) }
          notMovedLeftPlayer  = player { velocity = V2 (x $ velocity player + 5) (y $ velocity player) }

inputHandler _ state = state

pythonInputHandler :: Int -> GameState -> GameState
pythonInputHandler inputId state
      -- nothing, keep previous input
      | 0                           <- inputId
      = state

      -- up, keydown
      | 1                           <- inputId
      = changeEntity movedUpPlayer state

      -- up, keyup
      | -1                          <- inputId
      = changeEntity notMovedUpPlayer state

      -- right, keydown
      | 2                           <- inputId
      = changeEntity movedRightPlayer state

      -- right, keyup
      | -2                          <- inputId
      = changeEntity notMovedRightPlayer state

      -- down, keydown
      | 3                           <- inputId
      = changeEntity movedDownPlayer state

      -- down, keyup
      | -3                          <- inputId
      = changeEntity notMovedDownPlayer state

      -- left, keydown
      | 4                           <- inputId
      = changeEntity movedLeftPlayer state

      -- down, keyup
      | -4                          <- inputId
      = changeEntity notMovedLeftPlayer state

      where
          player           = head $ getEntitiesByClass state "Player"
        --   stationaryPlayer = player { velocity = V2 0 0 }
          movedUpPlayer       = player { velocity = V2 (x $ velocity player)     (y $ velocity player + 5) }
          movedDownPlayer     = player { velocity = V2 (x $ velocity player)     (y $ velocity player - 5) }
          movedRightPlayer    = player { velocity = V2 (x $ velocity player + 5) (y $ velocity player) }
          movedLeftPlayer     = player { velocity = V2 (x $ velocity player - 5) (y $ velocity player) }
          notMovedUpPlayer    = player { velocity = V2 (x $ velocity player)     (y $ velocity player - 5) }
          notMovedDownPlayer  = player { velocity = V2 (x $ velocity player)     (y $ velocity player + 5) }
          notMovedRightPlayer = player { velocity = V2 (x $ velocity player - 5) (y $ velocity player) }
          notMovedLeftPlayer  = player { velocity = V2 (x $ velocity player + 5) (y $ velocity player) }

pythonInputHandler _ state = state

inputHandlerIO :: Event -> GameState -> IO GameState
inputHandlerIO e s = return $ inputHandler e s

-- TODO: once velocity is added to entities, add the velocity of each entity to their position here
gameIterators :: Float -> GameState -> GameState
gameIterators t s = newState
    where
        movedEntities     = [ e { location = location e + velocity e } | e <- entities s ]
        movedParticles    = [ p { location = location p + velocity p } | p <- particles s ]
        movedState        = s { entities = movedEntities, particles = movedParticles, time = time s + t }
        prevT             = prevGenTime movedState
        newT              = time movedState
        noCollisionsState = removeCollidingCircles movedState
        prevEntities      = length $ entities s
        newHits           = length $ entities noCollisionsState
        hitCountedState   = noCollisionsState { hits = hits s + prevEntities - newHits }
        (e, _, genCircleState) = genFallingCircle hitCountedState (ySize + 20)
        addCircleState         = addEntity (e { entityClass = "FallingCircle" }) genCircleState
        newState | newT - prevT >= 0.3 = removeOutOfBoundsCircles addCircleState { prevGenTime = newT }
                 | otherwise           = removeOutOfBoundsCircles hitCountedState

gameIteratorsIO :: Float -> GameState -> IO GameState
gameIteratorsIO f s = do { return $ gameIterators f s }

checkFallingCircleCollision :: Entity -> Entity -> Bool
checkFallingCircleCollision player circle = d2D < d2DMax && ((d x < p x + r) || (d y < p y + r))
    where
        -- f attr1 attr2 param = attr1 $ attr2 param
        r      = x (size circle) / 2
        p dim  = dim (size player) / 2
        d dim  = abs (dim (location player) - dim (location circle))
        d2D    = sqrt (d x ** 2 + d y ** 2)
        d2DMax = sqrt (p x ** 2 + p y ** 2) + r

removeCollidingCircles :: GameState -> GameState
removeCollidingCircles s = removeEntitiesByCondition c s
    where
      players = getEntitiesByClass s "Player"
      c e = not (entityClass e /= "FallingCircle" || not (or [ checkFallingCircleCollision p e | p <- players ]))

removeOutOfBoundsCircles :: GameState -> GameState
removeOutOfBoundsCircles = removeEntitiesByCondition c
    where
      c e = entityClass e == "FallingCircle" && (x (location e) < - fALLING_CIRCLE_RADIUS || y (location e) < - fALLING_CIRCLE_RADIUS || x (location e) > xSize + fALLING_CIRCLE_RADIUS || y (location e) > ySize + fALLING_CIRCLE_RADIUS)

-- frame :: Picture
-- frame = undefined
