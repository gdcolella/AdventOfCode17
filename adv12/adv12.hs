import Data.Set (Set)

updateLine :: [Data.Set Int] -> (Int, [Int]) -> [Data.Set Int]
updateLine [] nextLine = snd nextLine
updateLine (st, rest) nextLine |
    fst nextLine