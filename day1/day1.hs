import System.IO
import System.Environment
import Data.Typeable

-- Compare each value in an array to its previous value
-- Returns a list of tuples containing the (value, increased, decreased, no-change)
compareValues :: (Ord a, Num a) => [a] -> [(a, Int, Int, Int)]
compareValues (x:y:xs)
  | x == 0 = [(y, 0, 0, 0)] ++ compareValues ([y]++xs)
  | y > x = [(y, 1, 0, 0)] ++ compareValues ([y]++xs)
  | y < x = [(y, 0, 1, 0)] ++ compareValues ([y]++xs)
  | y == x = [(y, 0, 0, 1)] ++ compareValues ([y]++xs)
  | otherwise = compareValues ([y]++xs)
compareValues _ = []

-- Compare the sum of windows of values
-- i.e. in the array: [x, y, z, a, ...], compare x+y+z vs y+z+a
compareValuesWithWindow :: (Ord a, Num a) => [a] -> [(a, Int, Int, Int)]
compareValuesWithWindow (x:y:z:a:xs)
  | x == 0 = [(secondSum, 0, 0, 0)] ++ compareValuesWithWindow ([y,z,a]++xs)
  | secondSum > firstSum = [(secondSum, 1, 0, 0)] ++ compareValuesWithWindow ([y,z,a]++xs)
  | secondSum < firstSum = [(secondSum, 0, 1, 0)] ++ compareValuesWithWindow ([y,z,a]++xs)
  | secondSum == firstSum = [(secondSum, 0, 0, 1)] ++ compareValuesWithWindow ([y,z,a]++xs)
  | otherwise = compareValuesWithWindow ([y,z,a]++xs)
  where secondSum = (y + z + a)
        firstSum = (x + y + z)
compareValuesWithWindow _ = []

-- Choose the correct function for the task (1 or 2)
compareValuesForTask :: (Ord a, Num a) => (String) -> [a] -> [(a, Int, Int, Int)]
compareValuesForTask task l
  | task == "1" = compareValues l
  | task == "2" = compareValuesWithWindow l
  | otherwise = compareValues l


-- Map a tuple to a string
mapTuple :: (Int, Int, Int, Int) -> String
mapTuple (x, 0, 0, 0) = show x ++ " (N/A - no previous measurement)"
mapTuple (x, 1, 0, 0) = show x ++ " (increased)"
mapTuple (x, 0, 1, 0) = show x ++ " (decreased)"
mapTuple (x, 0, 0, 1) = show x ++ " (no change)"
mapTuple (x, inc, dec, noChange) = show x

-- Count the number of values that were increases over the previous value
getIncreasedCount :: [(Int, Int, Int, Int)] -> Int -> Int
getIncreasedCount [] acc = acc
getIncreasedCount ((_, i, _, _):xs) acc = getIncreasedCount xs (i + acc)


-- Usage:
-- runhaskell day1.hs 1
-- or
-- runhaskell day1.hs 2
main = do
  (task:args) <- getArgs
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  -- Read in the file and cast the elements to Ints
  let linesOfFile = lines contents
  let listOfInts = map (read::String->Int) linesOfFile

  -- Prepending the "0" to the list is a bit of a hack but it makes our pattern matching in compareValues easier
  let comparedContents = compareValuesForTask task ([0] ++ listOfInts)
  
  -- Map the tuples back to Strings
  let listOfStrings = map mapTuple comparedContents

  -- Convert the List of Strings to a String
  let output = unlines listOfStrings

  -- Get the number of times the value increased (initialise the accumulator with 0)
  let numOfIncreased = getIncreasedCount comparedContents 0

  putStr output
  putStr ((show::Int->String) numOfIncreased)
  hClose handle
