import Debug.Trace

calcHash :: [Int] -> Int
calcHash items = (head items) * (head (tail items))


reverseLists :: ([Int], [Int]) -> ([Int], [Int])
reverseLists (head, end) =
    (newhead, newtail)
    where
        newtail = take (length head) flipped
        newhead = drop (length head) flipped 
        flipped = (reverse (end++head))

subLists :: [Int] -> Int -> Int -> ([Int], [Int], [Int])
subLists items start len =
    (header, middle, footer)
    where footer = drop start (take (start+len) items)
          headamount = (start+len) - (length items)
          header = if headamount > 0 then (take headamount items) else []
          middle = take ((length items) - (length header) - (length footer)) (drop (length header) items)
    

doFlip :: [Int] -> Int -> Int -> [Int]
doFlip items length currentPos =
    header ++ middle ++ footer
    where trace (show (ohead, middle, ofoot)) (ohead, middle, ofoot) = subLists items currentPos length
          (header, footer) = reverseLists (ohead, ofoot)

    
executeHash:: [Int] -> [Int] -> Int -> Int -> [Int]

executeHash items _ _ _ | trace (show items) False = undefined

executeHash items [] _ _ = items
executeHash items lengths currentPos skipLength = 
    executeHash (doFlip items len currentPos) remaining nextPos (succ skipLength)
    where len = head lengths
          remaining = tail lengths
          nextPos = (currentPos + skipLength + len) `mod` (length items)
    


main = do
    let list = [0,1,2,3,4]
    let lengths = [3,4,1,5]
    print $ executeHash list lengths 0 0
    
