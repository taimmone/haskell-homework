import Data.List
import Data.Maybe

charVals :: [(Char,Int)]
charVals = filter (odd . snd) $ zip ['a'..'z'] [1..]

chars :: String
chars = map fst charVals

values :: [Int]
values = map snd charVals

f n (x:xs) = if find (==product') xs /= Nothing then product' : f n xs else []
    where product' = n * x

g [] = []
g l@(x:xs) = if x == 1
             then g xs
             else (f x l) : g xs

h = [fst c | c <- charVals, find (==(snd c)) (concat $ g values) /= Nothing]


-- [
--     (3,3) i
--     (3,5) o
--     (3,7) u
--     /(3,9)
--     (5,5) y
--     /(5,7)
-- ]