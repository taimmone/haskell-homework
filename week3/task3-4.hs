fListC :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
fListC f d z ss = [ x | x <- ss, f z x <= d ]

-- similarStrings :: (String-> String -> Float) -> Float -> [String] -> [[String]]
-- similarStrings _ _ [] = []
-- similarStrings f d ss@(x:xs) = 