import Data.List
import Data.Maybe

oddChars :: [(Char,Int)]
oddChars = filter (odd . snd) $ zip ['a'..'z'] [1..]

chars :: String -- call this for the first part
chars = map fst oddChars

values :: [Int]
values = map snd oddChars

-- Second part, a bit of a mess
h = [fst c | c <- oddChars, find (==(snd c)) (concat $ g values) /= Nothing] -- "iouy"

f n (x:xs) = if find (==product') xs /= Nothing then product' : f n xs else []
    where product' = n * x

g [] = []
g l@(x:xs) = if x == 1
             then g xs
             else (f x l) : g xs