module DistanceAtMost
( fRec
, fListC
, fFoldl
, fFilter
) where
    
import Data.List

-- a)
fRec :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fRec _ _ _ [] = []
fRec f d z (x:xs) = if f z x <= d then x : fRec f d z xs else fRec f d z xs

-- b)
fListC :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fListC f d z ss = [ x | x <- ss, f z x <= d ]

-- c)
fFoldl :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fFoldl f d z ss = foldl (\acc x -> if f z x <= d then acc ++ [x] else acc ) [] ss 

-- d)
fFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fFilter f d z ss = filter (\x -> f z x <= d) ss

-- Task 3.2 functions
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