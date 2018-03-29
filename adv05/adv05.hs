import qualified Data.Map.Strict as Map
import Data.Maybe

step :: Int -> Map.Map Int Int -> Int
step val tape = 
    let lookedUp = Map.lookup val tape
        newTape = Map.adjust succ val tape
    in if isJust lookedUp then 1 + step (val+fromJust lookedUp) newTape else 0

inter :: String -> Int
inter s = read s

main = do
    s <- getContents
    let tape2 = Map.fromList (zip [0..] (map inter (lines s)))
    print $ step 0 tape2