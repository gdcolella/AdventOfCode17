import Data.Set

hasdup :: [String] -> Bool
hasdup wrds = length wrds == length (Data.Set.fromList(wrds))

cleanInpt :: String -> [[String]]
cleanInpt allIn = Prelude.map words (lines allIn)

numPass :: String -> Int
numPass allin = sum (Prelude.map (fromEnum . hasdup) (cleanInpt allin))


main = do
    s <- getContents
    print $ numPass s

