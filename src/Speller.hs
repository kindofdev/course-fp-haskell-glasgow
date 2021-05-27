module Speller(speller) where

speller :: [[Char]] -> [Char]
speller [[]]   = ""
speller words_ = joiner comma and_ phrases
  where comma = ", "
        and_ = "and "
        phrases = map letterPhrase words_

letterPhrase :: [Char] -> [Char]
letterPhrase []           = []
letterPhrase word @ (x:_) = [x] ++ " is for " ++ word  

joiner :: [a] -> [a] -> [[a]] -> [a]
joiner _ _ []            = []
joiner _ and_ [x]        = and_ ++ x  
joiner comma and_ (x:xs) = x ++ comma ++ (joiner comma and_ xs)     
