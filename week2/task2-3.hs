count :: (Char,Char) -> Int -> String -> Int
count _ _ [] = 0
count (c1,c2) g st = lookThrough st
    where findSecond _ []     = 0
          findSecond 0 (x:_)  = if x == c2 then 1 else 0
          findSecond g (x:xs) = findSecond (g-1) xs
          lookThrough []     = 0
          lookThrough (x:xs) = 
            (if x == c1 then findSecond g xs else 0) + lookThrough xs
