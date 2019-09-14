myFunc :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
myFunc _ _ _ [] = []
myFunc f d z (x:xs) = if f z x <= d then x : myFunc f d z xs else myFunc f d z xs

myFunc' :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
myFunc' f d z ss = [ x | x <- ss, f z x <= d ]

myFunc'' :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
myFunc'' f d z ss = foldl (\acc x -> if f z x <= d then acc ++ [x] else acc ) [] ss 

myFunc''' :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
myFunc''' f d z ss = filter (\x -> f z x <= d) ss
