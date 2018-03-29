import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace


incPos :: State -> Int -> Int
incPos s pos = (pos+1) `mod` (length (banks s))

data State = State{ seen_set :: Set.Set [Int], banks :: Map.Map Int Int }



distribute :: State -> Int -> Int -> State
distribute state _ 0 = state
distribute state position amount = 
    distribute state{banks=newmap} new_pos (amount-1)
    where newmap = (Map.adjust succ position (banks state))
          new_pos = incPos state position

do_distribute :: State -> Int -> State
do_distribute state chosen = distribute state{banks=Map.insert chosen 0 (banks state)} (incPos state chosen) ((banks state) Map.! chosen)


pickBetweenItems :: (Int, Int) -> Int -> Int -> (Int, Int)
pickBetweenItems (-1,0) bank_number bank_value = (bank_number, bank_value)
pickBetweenItems (lowest_num, highest_value) bank_number bank_value =
    if bank_value > highest_value then (bank_number, bank_value)
    else 
        if bank_value == highest_value && bank_number < lowest_num then
            (bank_number, bank_value)
        else
            (lowest_num, highest_value)

pickForDist :: State -> Int
pickForDist (State _ banks) = let result = fst (Map.foldlWithKey pickBetweenItems (-1,0) banks)
            in result

flattenBanks :: State -> [Int]
flattenBanks (State _ banks) = map snd (Map.toAscList banks)

step :: State -> Int
step state = if Set.member (trace (show (flattenBanks state)) (flattenBanks state)) (seen_set state) then 0
             else 1 + (step (do_distribute new_state (pickForDist new_state)))
             where new_state = state{
                 seen_set = Set.insert (flattenBanks state) (seen_set state)
             }

make_state :: [Int] -> State
make_state items = 
    State{
        seen_set = Set.fromList [],
        banks = Map.fromList (zip [0..] items)
    }

main = do
    print $ "Running.."

    print $ step (make_state [0,2,7,0])
    print $ step (make_state [4,10,4,1,8,4,9,14,5,1,14,15,0,15,3,5])