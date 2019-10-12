-- Please compile this file, for example:
-- compile    ghc task6-3.hs
--     run    ./task6-3.hs

import System.IO
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

-- Some strings to simplify the main function
prompt   = "calc> "
quitCmd  = ["quit", ":q", "exit", "stop", "halt", "close"]
quitMsg  = "Quit command received. Bye!"
errorMsg = "Unable to calculate that."

-- Folds a list of integers with an arithmetic function matching the given operator
calculate :: String -> [Int] -> Maybe Int
calculate operator vals = case operator of "+" -> Just $ foldl1 (+) vals
                                           "-" -> Just $ foldl1 (-) vals
                                           "*" -> Just $ foldl1 (*) vals
                                           _   -> Nothing

calculation :: [String] -> Maybe Int
calculation xs
    | length xs /= 3 = Nothing -- The list must have only three elements, e.g. ["1","+","1"] or ["a","b,"c"]
calculation xs@(_:operator:_)  -- Give the arithmetic operator a name, safe pattern match thanks to the guard above
    | length maybeNums == 2 = calculate operator maybeNums -- If there are exactly two integers, calculate their result
    | otherwise = Nothing
    where maybeNums = mapMaybe (\x -> readMaybe x :: Maybe Int) xs -- Extract integers from the list

-- Usage: calc> 1 + 1 => 2
--        calc> 3 * 5 => 15
--        calc> 9 - 3 => 7
--        calc> hello => Unable to calculate that.
--        calc> quit  => Quit command received. Bye!    
main = do
    putStr prompt
    hFlush stdout
    line <- getLine
    if line `elem` quitCmd
        then putStrLn quitMsg
    else do
        putStrLn . maybe errorMsg show . calculation $ words line -- If Nothing, display errorMsg, else show result
        main
