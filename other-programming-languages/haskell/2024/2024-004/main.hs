import System.IO
import Data.List (transpose)

-- Define the target word and its length
targetWord :: String
targetWord = "XMAS"

wordLength :: Int
wordLength = length targetWord

-- Directions for moving in the grid
directions :: [(Int, Int)]
directions = 
    [ (0, 1)   -- Right
    , (1, 0)   -- Down
    , (1, 1)   -- Diagonal-right-down
    , (1, -1)  -- Diagonal-left-down
    , (0, -1)  -- Left
    , (-1, 0)  -- Up
    , (-1, -1) -- Diagonal-left-up
    , (-1, 1)  -- Diagonal-right-up
    ]

-- Function to check if a word exists in a given direction
checkWord :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
checkWord grid (x, y) (dx, dy) =
    let positions = take wordLength [(x + i * dx, y + i * dy) | i <- [0..]]
        isValid (r, c) = r >= 0 && c >= 0 && r < length grid && c < length (head grid)
        chars = [grid !! r !! c | (r, c) <- positions, isValid (r, c)]
    in length chars == wordLength && chars == targetWord

-- Count all occurrences of the target word
countOccurrences :: [[Char]] -> Int
countOccurrences grid =
    length [()
        | r <- [0..length grid - 1]
        , c <- [0..length (head grid) - 1]
        , dir <- directions
        , checkWord grid (r, c) dir]

-- Function to count all X-MAS patterns
countXmasPatterns :: [[Char]] -> Int
countXmasPatterns grid =
    length [()
        | r <- [1..length grid - 2]
        , c <- [1..length (head grid) - 2]
        , let center = grid !! r !! c
              topLeft = grid !! (r - 1) !! (c - 1)
              topRight = grid !! (r - 1) !! (c + 1)
              bottomLeft = grid !! (r + 1) !! (c - 1)
              bottomRight = grid !! (r + 1) !! (c + 1)
        , center == 'A'
        , (topLeft, topRight, bottomLeft, bottomRight) `elem`
            [ ('M', 'S', 'M', 'S')
            , ('S', 'M', 'S', 'M')
            , ('M', 'M', 'S', 'S')
            , ('S', 'S', 'M', 'M')
            ]]

-- Main function to process the input file and compute results
main :: IO ()
main = do
    -- Read the grid from the input file
    contents <- readFile "input.txt"
    let grid = lines contents

    -- Count occurrences of the word "XMAS"
    let wordCount = countOccurrences grid
    putStrLn $ "count is " ++ show wordCount

    -- Count all X-MAS patterns
    let xmasPatternCount = countXmasPatterns grid
    putStrLn $ "total xmas patterns is " ++ show xmasPatternCount
