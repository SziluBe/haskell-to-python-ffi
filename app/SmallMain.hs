module SmallMain where
      
-- import Foreign.C.Types ()
import Foreign.C.String ( CString, newCString, peekCString )

import Data.Either ( fromRight )
import Data.Maybe ( fromMaybe )
import System.Random ( newStdGen )
-- import Codec.Picture
--     ( readPng,
--       dynamicMap,
--       generateImage,
--       DynamicImage(ImageY8),
--       Image(imageWidth, imageHeight) )
-- import Graphics.Gloss
--     ( circle, scale, Picture(Text), Color, green, Display(InWindow) )
import Graphics.Gloss.Data.Picture ( circle, scale, Picture(Text) )
-- import GlossJuicy ( loadJuicyPNG )
-- import Graphics.Gloss.Interface.Pure.Simulate ()
import Graphics.Gloss.Interface.IO.Game
    ( circle, Color, green, Display(InWindow), playIO )
-- import Graphics.Gloss.Interface.Pure.Game
--     ( Key(SpecialKey),
--       KeyState(Up, Down),
--       SpecialKey(KeySpace),
--       Event(EventKey) )

import LinAlg ( V2(V2) )
import Entity ( Entity(Entity), baseEntity, sprite )
import GameState
    ( addEntity,
      initGameState,
      stateToPicIO,
      xSize,
      ySize,
      GameState(randomGen) )
import RandEntityGenerator ( genEntityAtRandX )
import Play ( gameIteratorsIO, inputHandlerIO )

-- window parameters
width, height, offset :: Int
width  = round xSize
height = round ySize
offset = 0

-- dynWidth :: DynamicImage -> Int
-- dynWidth = dynamicMap imageWidth

-- dynHeight :: DynamicImage -> Int
-- dynHeight = dynamicMap imageHeight

-- loadPicWithDims :: String -> IO (Picture, Int, Int)
-- loadPicWithDims fileName = do { loadFrame <- readPng fileName
--       ; let img = fromRight (ImageY8 (generateImage (\a b -> 0) 1 1)) loadFrame
--       ; let imgWidth = dynWidth img
--       ; let imgHeight = dynHeight img
--       ; maybeFrame <- loadJuicyPNG fileName
--       ; let frame = fromMaybe (circle 20) maybeFrame 
--       ; return (frame, imgWidth, imgHeight) }

-- Important functions for Pictures: translate, Pictures (constructor), scale, 

main :: IO ()
-- main = play window bgColor fps gameState stateToPic inputHandler gameIterators
main = do {
      --      (frame, imgWidth, imgHeight) <- loadPicWithDims "sprites/swole-doge.png"
      --     ; let dogeEntity = Entity 
      --                               0
      --                               "Player"
      --                               (V2 0 0) 
      --                               (V2 (0.2 * fromIntegral imgWidth) (0.2 * fromIntegral imgHeight))
      --                               (V2 0 0)
      --                               $ scale 0.2 0.2 frame
          ; rand <- newStdGen
          ; let gameState = (addEntity ( baseEntity { sprite = circle 20 } ) initGameState) { randomGen = rand }
      --     ; let gameState = (addEntity dogeEntity initGameState) { randomGen = rand }
          ; let (_, _, randTopEntityState) = genEntityAtRandX gameState (ySize - 40)
      --     ; display window bgColor $ stateToPic gameState }
      --     ; display window bgColor (Pictures [translate (-100) (-100) frame, translate 100 100 $ Text $ show (imgWidth, imgHeight)]) }
      --     ; animate window bgColor animTestFunc }
      --     ; simulate window bgColor 1 simTestModel animTestFunc simUpdateTest }
      --     ; play window bgColor 1 simTestModel animTestFunc eventTest simUpdateTest }
          ; playIO window bgColor fps randTopEntityState stateToPicIO inputHandlerIO gameIteratorsIO }

-- eventTest :: Event -> (Float, Float) -> (Float, Float)
-- eventTest e (t, b)
--       | EventKey (SpecialKey KeySpace) Down _ _ <- e
--       = (t, 1)
      
--       | EventKey (SpecialKey KeySpace) Up _ _ <- e
--       = (t, 0)

--       | otherwise
--       = (t, 0)

-- simUpdateTest :: ViewPort -> Float -> (Float, Float) -> (Float, Float)
-- simUpdateTest :: Float -> (Float, Float) -> (Float, Float)
-- simUpdateTest _ (t, b) | b == 0 = (t + 1, 0)
--                        | otherwise = (t, 1)

-- simTestModel :: (Float, Float)
-- simTestModel = (0, 0)

-- animTestFunc :: (Float, Float) -> Picture
-- animTestFunc = Text . show

window :: Display
window = InWindow "Bullethell" (width, height) (offset, offset)

bgColor :: Color
bgColor = green

fps :: Int
fps = 60



hello :: CString -> IO CString
hello w 
 = do
   s <- peekCString w       
   newCString (s ++ "World!")
   
foreign export stdcall hello :: CString -> IO CString