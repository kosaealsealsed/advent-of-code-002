import System.IO
import Data.List (foldl')
import qualified Data.Map as Map

-- Main function to calculate similarity score
main :: IO ()
main = do
    -- File path
    let filePath = "input.txt"
    
    -- Read and parse the file
    content <- readFile filePath
    let (leftList, rightList) = parseFile content
    
    -- Create a frequency map for the right list
    let rightListCounts = createFrequencyMap rightList
    
    -- Calculate the similarity score
    let similarityScore = calculateSimilarityScore leftList rightListCounts
    
    -- Print the result
    putStrLn $ "Similarity Score: " ++ show similarityScore

-- Parse the file content into two lists
parseFile :: String -> ([Int], [Int])
parseFile content = 
    let linesList = lines content
        parseLine line = let [left, right] = map read (words line) in (left, right)
        (lefts, rights) = unzip $ map parseLine linesList
    in (lefts, rights)

-- Create a frequency map for a list of integers
createFrequencyMap :: [Int] -> Map.Map Int Int
createFrequencyMap = foldl' (\acc x -> Map.insertWith (+) x 1 acc) Map.empty

-- Calculate the similarity score
calculateSimilarityScore :: [Int] -> Map.Map Int Int -> Int
calculateSimilarityScore leftList rightListCounts =
    sum $ map (\left -> left * Map.findWithDefault 0 left rightListCounts) leftList
