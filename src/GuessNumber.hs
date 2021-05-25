module GuessNumber (
  guessNumber
) where 

import System.Random
import Data.List (intercalate)

-- Guess a number given between 0 and an upperBound 
-- Show if the number entered is lower or higher than the number to guess

type History = [String]

guessNumber :: Int -> Int -> IO ()
guessNumber upperBound attempts = do
  number <- randomRIO (0, upperBound)
  turn number initialHistory attempts False

check :: Int -> History -> Int -> (Bool, History)
check number history n = (comp == EQ, history')
  where comp = compare n number
        history' = tip comp n : history

tip :: Ordering -> Int -> String
tip GT n = mconcat["         <----- ", show n]
tip LT n = mconcat[show n, " ----->        "]
tip EQ n = mconcat["!----- ", show n, " -----!"]

turn :: Int -> History -> Int -> Bool -> IO ()
turn number history attempts guessed | guessed     = putStrLn "You win!"
                                     | attempts==0 = putStrLn "You lose" >> putStrLn ("The number is : " ++ show number)
                                     | otherwise   = mkguess number history attempts

mkguess :: Int -> History -> Int -> IO ()
mkguess number history attempts = do
  putStrLn $ "Attempts left:  " ++ replicate attempts '*' ++ "\n"
  putStrLn $ showHistory history
  putStrLn "Enter your guess: "
  n <- read <$> getLine
  let (correct, history') = check number history n
  let attempts' = if correct then attempts else attempts-1
  turn number history' attempts' correct

showHistory :: History -> String
showHistory = intercalate "\n" . reverse

initialHistory :: History
initialHistory = []