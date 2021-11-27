module Entity where
      
import Foreign.C.Types ()
import Foreign.C.String ( CString, newCString, peekCString )

import Graphics.Gloss (Picture, circle)

import LinAlg

-- maybe add acceleration? Probably not tho
-- velocity should be coded as for example 100/fps, which would mean 100 pixels per second
-- more generally, x/fps, so the code is more readable, because x is pixels per second
data Entity = Entity
    { entityID    :: Int
    , entityClass :: EntityClass
    , location    :: V2
    , size        :: V2
    , velocity    :: V2
    , sprite      :: Picture
    }
    | NullEntity

baseEntity :: Entity
baseEntity = Entity 0 "DefaultEntity" (V2 0 0) (V2 1 1) (V2 0 0) $ circle 1

type Particle = Entity

type EntityClass = String



-- hello :: CString -> IO CString
-- hello w 
--  = do
--    s <- peekCString w       
--    newCString (s ++ "World!")
   
-- foreign export stdcall hello :: CString -> IO CString

adder :: Int -> Int -> IO Int
adder x y = return (x+y)

foreign export stdcall adder :: Int -> Int -> IO Int