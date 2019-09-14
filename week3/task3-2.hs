module StringLen
( f
, g
) where
import Data.List

f :: String -> String -> Float
f [] [] = 0
f xs ys = 
    let xCharsNotInY = fromIntegral . length . snd $ partition (`elem` ys) xs
        yCharsNotInX = fromIntegral . length . snd $ partition (`elem` xs) ys
        lengthSum    = fromIntegral $ length xs + length ys
    in  (xCharsNotInY + yCharsNotInX) / lengthSum

g :: String -> String -> Float
g [] [] = 0
g xs ys =
    let xCharsNotNums = fromIntegral . length . snd $ partition (`elem` ['0'..'9']) xs
        yCharsNotNums = fromIntegral . length . snd $ partition (`elem` ['0'..'9']) ys
        lengthSum     = fromIntegral $ length xs + length ys
    in  (xCharsNotNums + yCharsNotNums) / lengthSum