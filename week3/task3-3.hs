fRec :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fRec _ _ _ [] = []
fRec f d z (x:xs) = if f z x <= d then x : fRec f d z xs else fRec f d z xs

fListC :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fListC f d z ss = [ x | x <- ss, f z x <= d ]

fFoldl :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fFoldl f d z ss = foldl (\acc x -> if f z x <= d then acc ++ [x] else acc ) [] ss 

fFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fFilter f d z ss = filter (\x -> f z x <= d) ss
