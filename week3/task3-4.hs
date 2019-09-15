import Data.List

fListC :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fListC f d z ss = [ x | x <- ss, f z x <= d ]

similarStrings :: (String -> String -> Float) -> Float -> [String] -> [[String]]
similarStrings _ _ [] = []
similarStrings f d ss = map (\x -> fListC f d x ss) ss

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