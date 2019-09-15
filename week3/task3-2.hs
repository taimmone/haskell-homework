module Distance
( distance
, distance'
) where
import Data.List

-- a)
distance :: String -> String -> Float
distance [] [] = 0
distance xs ys = 
    let xCharsNotInY = fromIntegral . length . snd $ partition (`elem` ys) xs
        yCharsNotInX = fromIntegral . length . snd $ partition (`elem` xs) ys
        lengthSum    = fromIntegral $ length xs + length ys
    in  (xCharsNotInY + yCharsNotInX) / lengthSum

-- b)
distance' :: String -> String -> Float
distance' [] [] = 0
distance' xs ys =
    let xCharsNotNums = fromIntegral . length . snd $ partition (`elem` ['0'..'9']) xs
        yCharsNotNums = fromIntegral . length . snd $ partition (`elem` ['0'..'9']) ys
        lengthSum     = fromIntegral $ length xs + length ys
    in  (xCharsNotNums + yCharsNotNums) / lengthSum