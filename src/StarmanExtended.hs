module StarmanExtended (
  starman
) where 

import System.Random
    
-- Add file dictionary 
-- Show words entered
-- Does not check bad input

starman :: FilePath -> Int -> IO ()
starman dictionary n = do 
  word <- readWord dictionary 
  turn word ['-' | _ <- word] n ""

readWord :: FilePath -> IO String 
readWord file = do
  contents <- readFile file
  let _lines = lines contents
  let l = length _lines
  i <- randomRIO (0,l)
  return $ _lines !! i  

check :: String -> String -> Char -> (Bool, String)
check word display c = (guessed, newDisplay)
  where guessed = c `elem` word
        newDisplay = [ if x==c then c else y | (x,y) <- zip word display]

turn :: String -> String -> Int -> [Char] -> IO ()
turn word display n entered | n==0            = putStrLn "You lose" >> putStrLn ("The word was : " ++ word)
                            | word == display = putStrLn "You win!"
                            | otherwise       = mkguess word display n entered   

mkguess :: String -> String -> Int -> [Char] -> IO ()
mkguess word display n entered = do
  putStrLn $ display ++ "       " ++ replicate n '*'
  putStrLn $ "You have entered: [" ++ entered ++ "]"
  putStrLn "Enter your guess: "
  q <- getLine
  let letter = q !! 0
  let (correct, display') = check word display letter                   
  let n' = if correct then n else n-1
  turn word display' n' (letter:entered)

