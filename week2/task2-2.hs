validateIBAN :: String -> Bool
validateIBAN st =
    if take 2 st == "FI"
    && length st == (18)
    && (\(x:_) -> x `elem` ['0'..'9']) digits
    then decimal `mod` 97 == 1
    else False
    where digits = drop 2 st
          suffix = "1518" ++ (take 2 digits)
          decimal = read $ drop 2 digits ++ suffix :: Integer