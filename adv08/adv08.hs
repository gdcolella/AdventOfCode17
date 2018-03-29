import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.Maybe
import Debug.Trace

type Cond = Int -> Int -> Bool

data State = State{
    registers :: Map.Map String Int,
    maxValue :: Int
} deriving Show

data Instruct = Instruct{
    dest :: String,
    amount :: Int,
    check :: String,
    cond :: Cond,
    compare_amt :: Int
}

getReg :: State -> String -> Int
getReg startState reg = fromMaybe 0 (Map.lookup reg (registers startState))

shouldDo :: State -> Instruct -> Bool
shouldDo state Instruct{dest=d, amount=_, check=c, cond=cnd, compare_amt=comp} = 
    cnd (getReg state c) comp

incReg :: State -> String -> Int -> State
incReg state reg change = state{
    registers=Map.insert reg ((getReg state reg)+change) (registers state)
}

maxReg :: State -> Int
maxReg state = Map.foldl max 0 (registers state)

executeInst :: State -> Instruct -> State
executeInst state inst 
    | doIt = incReg new_state (dest inst) (amount inst)
    | otherwise = new_state
    where doIt = shouldDo (trace (show new_state) new_state) inst
          new_state = state{maxValue= max (maxValue state) (maxReg state)}

runAll :: [Instruct] -> (Int, Int) 
runAll instructions = (maxReg ending, maxValue ending) 
                      where ending = (foldl executeInst State{registers=Map.fromList [], maxValue=0} instructions)


readMod :: String -> Int
readMod "inc" = 1
readMod "dec" = -1

readAmt :: String -> String -> Int
readAmt modifier amount = (readMod modifier) * (read amount)

packInst :: [String] -> Instruct
packInst [dst, change, delta, _, checkreg, condit, checkamount] = Instruct{
    dest=dst,
    amount=readAmt change delta,
    check=checkreg,
    cond=readCond condit,
    compare_amt=read checkamount
}
packInst x = Instruct{}

readCond :: String -> Cond
readCond ">" = (>)
readCond "<" = (<)
readCond ">=" = (>=)
readCond "<=" = (<=)
readCond "==" = (==)
readCond "!=" = (/=)

readInstLine :: String -> Instruct
readInstLine line = packInst (splitOn " " line)

main = do
    inpt <- getContents
    let instructions = map readInstLine (lines inpt)
    print $ runAll instructions