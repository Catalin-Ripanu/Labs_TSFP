module Pics where  -- (2p)

import Data.List (intercalate)

{-  
    The 'Pic' type designates images.
    All image manipulating functions are to be implemented using
    POINT-FREE STYLE.
    If this is unfamiliar to you, please request assistance :).
    
    Adapted from 'S. Thompson (2011). �Haskell: The Craft of Functional
    Programming� (3rd edition). Addison Wesley'.
-}

type Pic = [String] 

cross :: Pic
cross = [ ".#..."
        , "#####"
        , ".#..."
        , ".#..."
        , ".#..."
        ]

{-
    Use 'printPic <somePic>' to nicely display the pic at the console.
-}

printPic :: Pic -> IO ()
printPic = putStrLn . intercalate "\n"

{-
    1. (0.3p)
    Flip along the horizontal axis.
-}

myReverse :: [a] -> [a]
myReverse = foldr (\x acc -> acc ++ [x]) []

flipH :: Pic -> Pic
flipH = myReverse

{-
    2. (0.3p)
    Flip along the vertical axis.
-}

flipV :: Pic -> Pic
flipV = map myReverse

{-
    3. (0.3p)
    Rotate 180 degrees.
-}

rotate :: Pic -> Pic
rotate = flipV . flipH

{-
    4. (0.3p)
    Place the first pic on top of the second.
-}

above :: Pic -> Pic -> Pic
above = (++)

{-
    5. (0.3p)
    Place the first pic to the left of the second.
-}

beside :: Pic -> Pic -> Pic
beside = zipWith (++)

{-
    6. (0.5p)
    Invert 'colors'.
-}

invert :: Pic -> Pic
invert = let invert '.' = '#'
             invert '#' = '.'
             in map $ map invert