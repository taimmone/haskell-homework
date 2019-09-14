import Data.List
import Data.Maybe

charVals :: [(Char,Int)]
charVals = filter (odd . snd) $ zip ['a'..'z'] [1..]

c :: String
c = map fst charVals

v :: [Int]
v = map snd charVals



-- [
--     (3,3) i
--     (3,5) o
--     (3,7) u
--     /(3,9)
--     (5,5) y
--     /(5,7)
-- ]