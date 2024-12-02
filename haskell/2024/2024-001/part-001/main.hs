import System.IO
import Data.List (sort)
import Control.Monad (liftM2)

-- Function to read the input file and calculate total distance
main :: IO ()
main = do
    -- File path
    let filePath = "input.txt"
    
    -- Read and parse the file
    content <- readFile filePath
    let (leftList, rightList) = parseFile content
    
    -- Sort both lists
    let sortedLeft = sort leftList
    let sortedRight = sort rightList
    
    -- Calculate total distance
    let totalDistance = calculateTotalDistance sortedLeft sortedRight
    
    -- Print the result
    putStrLn $ "Total Distance: " ++ show totalDistance

-- Parse the file content into two lists
parseFile :: String -> ([Int], [Int])
parseFile content = 
    let linesList = lines content
        parseLine line = let [left, right] = map read (words line) in (left, right)
        (lefts, rights) = unzip $ map parseLine linesList
    in (lefts, rights)

-- Calculate the total distance between two sorted lists
calculateTotalDistance :: [Int] -> [Int] -> Int
calculateTotalDistance leftList rightList = 
    sum $ zipWith (\left right -> abs (left - right)) leftList rightList
