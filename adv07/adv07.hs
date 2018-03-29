import Data.List.Split
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace


data Node = Node{
    name :: String,
    children :: [String],
    weight :: Int
} deriving Show

data Graph = Graph{
    nodes :: Map.Map String Node
}

parseName :: String -> String
parseName line = (head (splitOn " " line))

parseWeight :: String -> Int
parseWeight line = read (splitOn ")" ((splitOn "(" line) !! 1) !! 0)

parseChildren :: String -> [String]
parseChildren line
    | ending == [] = []
    | otherwise    = splitOn ", " (head ending)
    where ending = tail (splitOn "-> " line)

parseLine :: String -> Node
parseLine line = Node{name = parseName line, children = parseChildren line}


findRoot :: Set.Set String -> Node -> Set.Set String
findRoot potential_roots new_node =
    Set.difference potential_roots (Set.fromList (children new_node))

allNodes :: [Node] -> Set.Set String
allNodes nodes = Set.fromList (map name nodes)

rootNode :: [Node] -> String
rootNode nodes =
    head (Set.toList (foldl findRoot (allNodes nodes) nodes))

main = do
    all_lines <- getContents
    let all_elements = map parseLine (lines all_lines)
    print $ all_elements
    print $ rootNode (all_elements)