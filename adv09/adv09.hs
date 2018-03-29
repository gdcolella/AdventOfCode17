garbage :: String -> Int
garbage ('>':rest) = groupScore rest
garbage ('!':rest) = garbage (tail rest)
garbage (_: rest) = 1+garbage rest

groupScore :: String -> Int
groupScore ('<':rest) = garbage rest
groupScore ('{':rest) = (groupScore rest)
groupScore ('}':rest) = groupScore rest
groupScore ('!': rest) = groupScore (tail rest)
groupScore (',': rest) = groupScore rest
groupScore (_: rest) = groupScore rest
groupScore [] = 0


main = do
    inpt <- readFile "inpt09"
    print $ groupScore inpt