module Speller(speller, speller') where

----- speller using recursion -----  
    
speller :: [[Char]] -> [Char]
speller [[]]   = ""
speller words_ = intercalate' separator phrases
  where separator = ", "
        phrases = map letterPhrase words_
  
letterPhrase :: [Char] -> [Char]
letterPhrase []           = []
letterPhrase word @ (x:_) = [x] ++ " is for " ++ word  
  
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ []       = []
intercalate' _ [x]      = x  
intercalate' sep (x:xs) = x ++ sep ++ (intercalate' sep xs)     

----- speller using fold -----

speller' :: [[Char]] -> [Char]
speller' [[]]   = ""
speller' words_ = endDrop (length separator) phasesFolded
  where separator = ", "
        phrases = map letterPhrase words_
        phasesFolded = foldr (foldFun separator) "" phrases

endDrop :: Int -> [a] -> [a]
endDrop n = (reverse . drop n . reverse)

foldFun :: [Char] -> [Char] -> [Char] -> [Char]
foldFun _ "" word          = word
foldFun separator acc word = acc ++ separator ++ word
