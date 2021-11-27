module GameState where

import System.Random ( StdGen, mkStdGen, randomR, randomRs )
-- import System.Random ( StdGen, UniformRange, uniformR )
import Graphics.Gloss
    ( Picture(Text, Blank, Pictures), circle, translate )
import Codec.Picture ()
import Graphics.Gloss.Interface.Pure.Game ()

import LinAlg
import Entity

xSize :: Float
xSize = 700

ySize :: Float
ySize = 700

fALLING_CIRCLE_RADIUS :: Float
fALLING_CIRCLE_RADIUS = 40

data GameState = State
    { dimensions      :: V2
    , worldPos        :: V2
    , entities        :: [Entity]
    , particles       :: [Particle]
    , background      :: Picture
    , states          :: [Bool] -- to note things down about the game, like whether the world being moved by the mouse, whether the game is paused, etc.
                           -- at some point it's probably be better to turn this list into a custom data type, or unpack it into different
                           -- fields of this one
    , highestEntityId :: Int
    , randomSeed      :: Int
    , idOfNextRandom  :: Int
    , time            :: Float
    , prevGenTime     :: Float
    , hits            :: Int
    }

gameStateToBasics :: GameState ->
                     ((Float, Float),
                      (Float, Float),
                      [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
                      [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
                      [Bool],
                      Int,
                      (Int, Int),
                      (Float, Float),
                      Int)
gameStateToBasics s = ((x $ dimensions s, x $ dimensions s),
                       (x $ worldPos s, y $ worldPos s),
                       [ entityToBasics a | a <- entities s ],
                       [ entityToBasics a | a <- particles s ],
                       states s,
                       highestEntityId s,
                       (randomSeed s, idOfNextRandom s),
                       (time s, prevGenTime s),
                       hits s
                       )
    where
        entityToBasics a = (entityID a, entityClass a, (x $ location a, y $ location a), (x $ size a, y $ size a), (x $ velocity a, y $ velocity a))

gameStateToBasicsIO :: IO GameState ->
                       IO ((Float, Float),
                           (Float, Float),
                           [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
                           [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
                           [Bool],
                           Int,
                           (Int, Int),
                           (Float, Float),
                           Int)
gameStateToBasicsIO gs = do { gameState <- gs
                            ; return (gameStateToBasics gameState) }

basicsToGameState :: ((Float, Float),
                      (Float, Float),
                      [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
                      [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
                      [Bool],
                      Int,
                      (Int, Int),
                      (Float, Float),
                      Int)
                      -> GameState
basicsToGameState s = State (ds s) (wp s) (es s) (ps s) bg (ss s) (he s) (rs s) (n s) (t s) (pt s) (hs s)
    where
        ds (a,_,_,_,_,_,_,_,_) = uncurry V2 a
        wp (_,a,_,_,_,_,_,_,_) = uncurry V2 a
        es (_,_,a,_,_,_,_,_,_) = [ basicsToEntity e | e <- a ]
        ps (_,_,_,a,_,_,_,_,_) = [ basicsToEntity e | e <- a ]
        bg = circle 500
        ss (_,_,_,_,a,_,_,_,_) = a
        he (_,_,_,_,_,a,_,_,_) = a
        -- ######################################################################
        -- ######################################################################
        -- ######################################################################
        rs (_,_,_,_,_,_,a,_,_) = fst a
        n  (_,_,_,_,_,_,a,_,_) = snd a
        -- ######################################################################
        -- ######################################################################
        -- ######################################################################
        t  (_,_,_,_,_,_,_,a,_) = fst a
        pt (_,_,_,_,_,_,_,a,_) = snd a
        hs (_,_,_,_,_,_,_,_,a) = a
        basicsToEntity :: (Int, EntityClass, (Float, Float), (Float, Float), (Float, Float)) -> Entity
        basicsToEntity (ei,ec,l,s,v) = Entity ei ec (uncurry V2 l) (uncurry V2 s) (uncurry V2 v) (circle (uncurry max s))

initGameState :: GameState
initGameState = State dims wPos initEntities [] Blank initStates 1 0 0 0 0 0

-- make sure there are no duplicate entity IDs when creating new entities!!
addEntity :: Entity -> GameState -> GameState
addEntity e s = s { entities = entities s ++ [e]
                  , highestEntityId = max (highestEntityId s) (entityID e) }

getEntityByID :: GameState -> Int -> Entity
getEntityByID s i | not $ null $ entities s = head $ [ e | e <- entities s, entityID e == i ]
                  | otherwise = NullEntity

getEntitiesByClass :: GameState -> EntityClass -> [Entity]
getEntitiesByClass s i | not $ null $ entities s = [ e | e <- entities s, entityClass e == i ]
                       | otherwise = []

removeEntityByID :: Int -> GameState -> GameState
removeEntityByID i s = s { entities = [ e | e <- entities s, entityID e /= i] }

removeEntitiesByClass :: String -> GameState -> GameState
removeEntitiesByClass c s = s { entities = [ e | e <- entities s, entityClass e /= c] }

removeEntitiesByCondition :: (Entity -> Bool) -> GameState -> GameState
removeEntitiesByCondition b s = s { entities = [ e | e <- entities s, not $ b e ] }

changeEntity :: Entity -> GameState -> GameState
changeEntity e s = addEntity e $ removeEntityByID i s
    where
        i = entityID e

-- getRandom :: UniformRange a => (a, a) -> GameState -> (a, GameState)
-- getRandom range s = (randNum, newState)
--     where
--         r        = uniformR range $ randomGen s
--         newState = s { randomGen = snd r }
--         randNum  = fst r

-- probably could have a faster version if I also stored the actual generator and had a separate function for random generation when called from python,
-- but I don't really need code inaccesible from python
getRandom :: (Float, Float) -> GameState -> (Float, GameState)
getRandom range s = (randNum, newState)
    where
        n        = idOfNextRandom s
        randNum  = (randomRs range $ mkStdGen $ randomSeed s) !! n -- nth element of rands of s
        newState = s { idOfNextRandom = n + 1 }

initStates :: [Bool]
initStates = [False]

wPos :: V2
wPos = V2 0 0

dims :: V2
dims = V2 xSize ySize

initEntities :: [Entity]
initEntities = []

testEntity1 :: Entity
testEntity1 = Entity 1 "DefaultEntity" (V2 0 0) (V2 40 40) (V2 0 0) $ circle 20

testEntity2 :: Entity
testEntity2 = Entity 2 "DefaultEntity" (V2 10 10) (V2 40 40) (V2 0 0) $ circle 20

stateToPic :: GameState -> Picture
stateToPic state = translate xTrans yTrans $ translate (-xSize/2) (-ySize/2) $ Pictures $ concat [bg, es, ps, [Text $ show $ hits state, translate 0 100 $ Text $ show $ length $ entities state]]
    where
        -- sprites func state = [ sprite x | x <- func state]
        es = [ translate (x $ location e) (y $ location e) $ sprite e | e <- entities state ]
        ps = [ translate (x $ location e) (y $ location e) $ sprite e | e <- particles state ]
        bg = [background state]
        transVec = worldPos state
        xTrans = x transVec
        yTrans = y transVec

stateToPicIO :: GameState -> IO Picture
stateToPicIO s = return $ stateToPic s
