import System.IO
import System.Environment
import Data.Typeable

-- Split "command x" into (command, x)
splitToTuple :: ([String]) -> (String, Int)
splitToTuple (x:y:xs) = (x, read y)
splitToTuple (x:xs) = (x, 0)
splitToTuple _ = ("", 0)

-- Change depth/horizontal position based on task parameters
moveSub :: [(String, Int)] -> Int -> Int -> Int
moveSub [] dep hor = dep * hor
moveSub ((cmd, v):xs) dep hor
  | cmd == "forward" = moveSub xs dep (hor + v)
  | cmd == "up" = moveSub xs (dep-v) hor
  | cmd == "down" = moveSub xs (dep+v) hor
  | otherwise = moveSub xs dep hor

-- Change depth/horizontal position/aim based on task parameters
moveSubWithAim :: [(String, Int)] -> Int -> Int -> Int -> Int
moveSubWithAim [] dep hor aim = dep * hor
moveSubWithAim ((cmd, v):xs) dep hor aim
  | cmd == "forward" = moveSubWithAim xs (dep + (aim*v)) (hor + v) aim
  | cmd == "up" = moveSubWithAim xs dep hor (aim-v)
  | cmd == "down" = moveSubWithAim xs dep hor (aim+v)
  | otherwise = moveSubWithAim xs dep hor aim


-- Choose the correct function for the task (1 or 2)
getFnForTask task l
  | task == "1" = moveSub l 0 0
  | task == "2" = moveSubWithAim l 0 0 0
  | otherwise = moveSub l 0 0


-- Usage:
-- runhaskell day2.hs 1
-- or
-- runhaskell day2.hs 2
main = do
  (task:args) <- getArgs
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle

  -- Read in the file and cast the elements to Ints
  let linesOfFile = lines contents
  let splitStrings = map words linesOfFile
  let commands = map splitToTuple splitStrings

  let moved = getFnForTask task commands
  
  putStr ((show::Int->String) moved)
  hClose handle
