module Main where
      
import Foreign.C.Types (CFloat (CFloat), CDouble (CDouble))
import Foreign.C.String ( CString, newCString, peekCString )

import FFI.Anything.TypeUncurry.Msgpack

import Data.Either ( fromRight )
import Data.Maybe ( fromMaybe )
import System.Random ( newStdGen, randomR, mkStdGen )
import Codec.Picture
    ( readPng,
      dynamicMap,
      generateImage,
      DynamicImage(ImageY8),
      Image(imageWidth, imageHeight) )
import Graphics.Gloss
    ( circle, scale, Picture(Text), Color, green, Display(InWindow) )
import Graphics.Gloss.Data.Picture ( circle, scale, Picture(Text) )
import GlossJuicy ( loadJuicyPNG )
import Graphics.Gloss.Interface.Pure.Simulate ()
import Graphics.Gloss.Interface.IO.Game
    ( Key(SpecialKey),
      KeyState(Up, Down),
      SpecialKey(KeySpace),
      Event(EventKey),
      playIO )
import Graphics.Gloss.Interface.Pure.Game
    ( Key(SpecialKey),
      KeyState(Up, Down),
      SpecialKey(KeySpace),
      Event(EventKey) )

import LinAlg ( V2(V2) )
import Entity ( Entity(Entity), baseEntity, sprite )
import GameState
--     ( GameState(randomGen, dimensions, worldPos, hits),
    ( GameState(dimensions, worldPos, hits),
      xSize,
      ySize,
      initGameState,
      addEntity,
      stateToPicIO,
      gameStateToBasics,
      gameStateToBasicsIO,
      basicsToGameState )
import RandEntityGenerator ( genEntityAtRandX )
import Play ( inputHandler, pythonInputHandler, inputHandlerIO, gameIterators, gameIteratorsIO )

-- window parameters
width, height, offset :: Int
width  = round xSize
height = round ySize
offset = 0

dynWidth :: DynamicImage -> Int
dynWidth = dynamicMap imageWidth

dynHeight :: DynamicImage -> Int
dynHeight = dynamicMap imageHeight

loadPicWithDims :: String -> IO (Picture, Int, Int)
loadPicWithDims fileName = do { loadFrame <- readPng fileName
      ; let img = fromRight (ImageY8 (generateImage (\a b -> 0) 1 1)) loadFrame
      ; let imgWidth = dynWidth img
      ; let imgHeight = dynHeight img
      ; maybeFrame <- loadJuicyPNG fileName
      ; let frame = fromMaybe (circle 20) maybeFrame 
      ; return (frame, imgWidth, imgHeight) }

-- Important functions for Pictures: translate, Pictures (constructor), scale, 

prep :: IO GameState
prep = do { (frame, imgWidth, imgHeight) <- loadPicWithDims "sprites/swole-doge.png"
          ; let dogeEntity = Entity 
                                   0
                                   "Player"
                                   (V2 0 0) 
                                   (V2 (0.2 * fromIntegral imgWidth) (0.2 * fromIntegral imgHeight))
                                   (V2 0 0)
                                   $ scale 0.2 0.2 frame
      --     ; rand <- newStdGen
      --     ; let gameState = (addEntity ( baseEntity { sprite = circle 20 } ) initGameState) { randomGen = rand }
      --     ; let gameState = (addEntity dogeEntity initGameState) { randomGen = rand }
          ; let gameState = (addEntity dogeEntity initGameState)
          ; let (_, _, randTopEntityState) = genEntityAtRandX gameState (ySize - 40)
          ; return randTopEntityState
          }

main :: IO ()
-- main = play window bgColor fps gameState stateToPic inputHandler gameIterators
main = do { randTopEntityState <- prep
      --     ; display window bgColor $ stateToPic gameState }
      --     ; display window bgColor (Pictures [translate (-100) (-100) frame, translate 100 100 $ Text $ show (imgWidth, imgHeight)]) }
      --     ; animate window bgColor animTestFunc }
      --     ; simulate window bgColor 1 simTestModel animTestFunc simUpdateTest }
      --     ; play window bgColor 1 simTestModel animTestFunc eventTest simUpdateTest }
          ; playIO window bgColor fps randTopEntityState stateToPicIO inputHandlerIO gameIteratorsIO }

eventTest :: Event -> (Float, Float) -> (Float, Float)
eventTest e (t, b)
      | EventKey (SpecialKey KeySpace) Down _ _ <- e
      = (t, 1)
      
      | EventKey (SpecialKey KeySpace) Up _ _ <- e
      = (t, 0)

      | otherwise
      = (t, 0)

-- simUpdateTest :: ViewPort -> Float -> (Float, Float) -> (Float, Float)
simUpdateTest :: Float -> (Float, Float) -> (Float, Float)
simUpdateTest _ (t, b) | b == 0 = (t + 1, 0)
                       | otherwise = (t, 1)

simTestModel :: (Float, Float)
simTestModel = (0, 0)

animTestFunc :: (Float, Float) -> Picture
animTestFunc = Text . show

window :: Display
window = InWindow "Bullethell" (width, height) (offset, offset)

bgColor :: Color
bgColor = green

fps :: Int
fps = 60



hello :: String -> String
hello s = s ++ "World!"
   
foreign export ccall hello_export :: CString -> IO CString
hello_export = export hello

getRandomNoState :: Float -> Float -> Float
getRandomNoState a b = fst $ randomR (a, b) $ mkStdGen 2

foreign export ccall getRandomNoState_export :: CString -> IO CString
getRandomNoState_export = export getRandomNoState

loadPicTest :: String -> IO Int
loadPicTest fileName = do {
      -- ; fileName <- peekCString cs
      -- ; print fileName
      ; loadFrame <- readPng fileName
      ; let img = fromRight (ImageY8 (generateImage (\a b -> 0) 1 1)) loadFrame
      ; let imgWidth = dynWidth img
      ; let imgHeight = dynHeight img
      ; maybeFrame <- loadJuicyPNG fileName
      ; let frame = fromMaybe (circle 20) maybeFrame 
      ; return imgWidth }

foreign export ccall loadPicTest_export :: CString -> IO CString
loadPicTest_export = exportIO loadPicTest

-- foreign export ccall inputHandler_export :: CString -> IO CString
-- inputHandler_export e s = export . gameStateToBasics . inputHandler (eventToBasics e) (gameStateToBasics s)


foreign export ccall gameIterators_export :: CString -> IO CString
gameIterators_export = export gameIterators_export_prep

-- gameIterators :: Float -> GameState -> GameState
gameIterators_export_prep f s = gameStateToBasics $ gameIterators f $ basicsToGameState s

foreign export ccall testGameState_export :: CString -> IO CString
testGameState_export = exportIO testGameState_export_prep

testGameState_export_prep = gameStateToBasicsIO $ prep

foreign export ccall pythonInputHandler_export :: CString -> IO CString
pythonInputHandler_export = export pythonInputHandler_export_prep

pythonInputHandler_export_prep n s = gameStateToBasics $ pythonInputHandler n $ basicsToGameState s




-- GameState basics:

-- ((Float, Float),
--  (Float, Float),
--  [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
--  [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
--  [Bool],
--  Int,
--  Int,
--  Int,
--  Float,
--  Float,
--  Int)





-- gameIterators_export_prep :: Float ->
--                              ((Float, Float),
--                               (Float, Float),
--                               [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
--                               [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
--                               [Bool],
--                               Int, 
--                               Float,
--                               Float,
--                               Int)
--                               -> 
--                                     ((Float, Float),
--                                      (Float, Float),
--                                      [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
--                                      [(Int, String, (Float, Float), (Float, Float), (Float, Float))],
--                                      [Bool],
--                                      Int, 
--                                      Float,
--                                      Float,
--                                      Int)





   
-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions

-- foreign export ccall worldPos_export :: CString -> IO CString
-- worldPos_export = export worldPos

-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions

-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions

-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions


-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions

-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions

-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions

-- foreign export ccall dimensions_export :: CString -> IO CString
-- dimensions_export = export dimensions

-- foreign export ccall hits_export :: CString -> IO CString
-- hits_export = export hits