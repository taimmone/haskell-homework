allNumbers :: String -> Bool
allNumbers [] = False
allNumbers [x] = x `elem` ['0'..'9']
allNumbers (x:xs) = allNumbers [x] && allNumbers xs