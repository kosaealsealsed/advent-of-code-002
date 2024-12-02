import Data.List (delete, subsequences)

-- Function to check if a report is safe
isSafe :: [Int] -> Bool
isSafe report =
  let differences = zipWith (-) (tail report) report
      allIncreasing = all (\diff -> diff >= 1 && diff <= 3) differences
      allDecreasing = all (\diff -> diff <= -1 && diff >= -3) differences
  in allIncreasing || allDecreasing

-- Function to check if a report is safe with the Problem Dampener
isSafeWithDampener :: [Int] -> Bool
isSafeWithDampener report
  | isSafe report = True
  | otherwise = any isSafe [take i report ++ drop (i + 1) report | i <- [0 .. length report - 1]]

-- Main function to process the input and count safe reports
main :: IO ()
main = do
  let inputPath = "input.txt"

  -- Read and process the input file
  contents <- readFile inputPath
  let reports = map (map read . words) (lines contents)

  -- Count safe reports
  let safeCount = length $ filter isSafe reports
  putStrLn $ "Safe reports: " ++ show safeCount

  -- Count safe reports with the Problem Dampener
  let safeWithDampenerCount = length $ filter isSafeWithDampener reports
  putStrLn $ "Safe reports with dampener: " ++ show safeWithDampenerCount
