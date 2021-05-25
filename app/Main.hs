module Main where

import System.Environment
import StarmanExtended
import GuessNumber

main :: IO ()
main = mainGuessNumber
--main = mainStarman


-- stack exec course-fp-haskell-glasgow-exe "./docs/words.txt" 5
-- Does not check bad input
mainStarman:: IO ()
mainStarman = do
  (file:attempts:_) <- getArgs
  starman file (read attempts)
  
  
-- stack exec course-fp-haskell-glasgow-exe 1000 5  
-- Does not check bad input
mainGuessNumber :: IO ()
mainGuessNumber = do
  (upperBound:attempts:_) <- getArgs
  guessNumber (read upperBound) (read attempts)  