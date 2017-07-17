--  File     : Proj1.hs
--  Author   : Hongyao Wei
--  ID       : 741027
--  Purpose  : Guessing cards game

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card

data GameState = GameState {guessLen :: Int, turn :: Int}

-- takes the number of cards in the answer as input and returns a pair of an initial guess,
-- which should be a list of the specified number of cards, and a game state. 
-- In this case only 2 cards condition is realized
initialGuess :: Int -> ([Card],GameState)
initialGuess n
     | n < 1 = initialGuess 1
     | n > 2 = initialGuess 2
     | otherwise = (answer n,gs)
         where gs = GameState{guessLen = n, turn = 0}

-- Grab a list of cards (1 or 2 cards)
answer :: Int -> [Card]
answer x = take x [Card Club R6, Card Club R10] 


-- takes a target and a guess (in that order), each represented as a list of Cards,
-- and returns the five feedback numbers
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback targetCard guessCard = (correct,lowerR,correctR,higherR,correctS) 
    where sortTC = quicksort targetCard
          sortGC = quicksort guessCard
          correct = correctCard sortTC sortGC
          lowerR  = lowerRank targetCard guessCard
          correctR = correctRank sortTC sortGC
          higherR = higherRank targetCard guessCard
          correctS = correctSuit sortTC sortGC


-- takes as input a pair of the previous guess and game state, 
-- and the feedback to this guess as a quintuple of counts of: 
-- correct cards, low ranks, correct ranks, high ranks, and correct suits, 
-- and returns a pair of the next guess and new game state.
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (pGuess,gs) (correct, lowerR, equalR, higherR, equalS) = 
     ([Card s1 r1, Card s2 r2], GameState len tur)
      where
         len = guessLen gs -- cards length
         tur 
             | lowerR == 0 && equalR == 1 && higherR == 0 = switchT (turn gs)
             | otherwise = turn gs

         -- r1 is lower rank card
         r1 
             | equalR == 2 = rank (pGuess !! 0)
             | lowerR == 0 && equalR == 0 && higherR == 2 = (highest pGuess)
             | lowerR == 0 && equalR == 1 && higherR == 1 = rank (pGuess !! 0)
             | lowerR == 1 && equalR == 0 && higherR == 1 = incRank (lowest pGuess) (-1)
             | lowerR == 1 && equalR == 1 && higherR == 0 = incRank (lowest pGuess) (-1)
             | lowerR == 2 && equalR == 0 && higherR == 0 = incRank (lowest pGuess) (-1)
             | lowerR == 0 && equalR == 0 && higherR == 0 = incRank (lowest pGuess) 1
             | lowerR == 0 && equalR == 1 && higherR == 0 = if (turn gs) == 0 then incRank (lowest pGuess) 1 else rank (pGuess !! 0)
             | lowerR == 1 && equalR == 0 && higherR == 0 = rank (pGuess !! 0)
             | lowerR == 0 && equalR == 0 && higherR == 1 = incRank (lowest pGuess) 1

         -- r2 is higher rank card
         r2 
             | equalR == 2 = rank (pGuess !! 1)
             | lowerR == 0 && equalR == 0 && higherR == 2 = incRank (highest pGuess) 1
             | lowerR == 0 && equalR == 1 && higherR == 1 = incRank (highest pGuess) 1
             | lowerR == 1 && equalR == 0 && higherR == 1 = incRank (highest pGuess) 1
             | lowerR == 1 && equalR == 1 && higherR == 0 = rank (pGuess !! 1)
             | lowerR == 2 && equalR == 0 && higherR == 0 = (lowest pGuess)
             | lowerR == 0 && equalR == 0 && higherR == 0 = incRank (highest pGuess) (-1)
             | lowerR == 0 && equalR == 1 && higherR == 0 = if (turn gs) == 1 then incRank (highest pGuess) (-1) else rank (pGuess !! 1)
             | lowerR == 1 && equalR == 0 && higherR == 0 = incRank (highest pGuess) (-1)
             | lowerR == 0 && equalR == 0 && higherR == 1 = rank (pGuess !! 1)

         -- s1 is suit of lower rank card
         s1
             | equalS == 0 = incSuit (suit (pGuess !! 0)) 1
             | equalS == 1 = suit (pGuess !! 0)
             | equalS == 2 && equalR == 2 && correct < 2 = suit (pGuess !! 1)
             | otherwise = suit (pGuess !! 0)

         -- s2 is suit of higher rank card	
         s2
             | equalS == 0 = incSuit (suit (pGuess !! 1)) 1
             | equalS == 1 = incSuit (suit (pGuess !! 1)) 1
             | equalS == 2 && equalR == 2 && correct < 2 = suit (pGuess !! 0)
             | otherwise = suit (pGuess !! 1)


-- ======== Some helper functions ========
-- Gets the lowest rank from a list of cards
lowest :: [Card] -> Rank
lowest cards = minimum (eR cards)

-- Gets the highest rank from a list of cards
highest :: [Card] -> Rank
highest cards = maximum (eR cards)

-- Extracts just the ranks from a list of cards and returns a list of the ranks
eR :: [Card] -> [Rank]
eR [] = []
eR (x:xs) = [(rank x)] ++ (eR xs) 

-- Extracts just the suits from a list of cards and returns a list of the suits
eS :: [Card] -> [Suit]
eS [] = []
eS (x:xs) = [(suit x)] ++ (eS xs) 


-- calculate how many of the cards in the answer are also in the guess
-- use quick sort function to format list at first 
correctCard :: [Card] -> [Card] -> Int
correctCard [] [] = 0
correctCard (x:xs) (y:ys) = c + correctCard xs ys
            where c = if rank x == rank y then 1 else 0


-- calculate how many cards in the answer have rank lower than the lowest rank in the guess
lowerRank :: [Card] -> [Card] -> Int
lowerRank [] [] = 0
lowerRank a b = length ([c| c <- eR a, c < lowest b])


-- calculate how many of the cards in the answer have the same rank as a card in the guess
-- one to one compare, match + 1, else 0
-- use quick sort function to format list at first 
correctRank :: [Card] -> [Card] -> Int
correctRank [] [] = 0
correctRank (x:xs) (y:ys) = c + correctRank xs ys
            where c = if rank x == rank y then 1 else 0


-- calculate how many cards in the answer have rank higher than the highest rank in the guess
higherRank :: [Card] -> [Card] -> Int
higherRank [] [] = 0
higherRank a b = length ([c| c <- eR a, c > highest b])


-- calculate how many of the cards in the answer have the same suit as a card in the guess
-- use quick sort function to format list at first 
correctSuit :: [Card] -> [Card] -> Int
correctSuit [] [] = 0
correctSuit (x:xs) (y:ys) = c + correctSuit xs ys
            where c = if suit x == suit y then 1 else 0

-- quick sort function
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
     let smaller = quicksort [a | a <- xs, a <= x]
         bigger = quicksort [a | a <- xs, a > x]
     in smaller ++ [x] ++ bigger

-- increase one rank of input card
incRank :: Rank -> Int -> Rank
incRank r x = toEnum (((fromEnum r) + x) `mod` 13)::Rank

-- increase one suit of input card
incSuit :: Suit -> Int -> Suit
incSuit s x = toEnum (((fromEnum s) + x) `mod` 4)::Suit

-- Switches turn
switchT x = case x of
     0 -> 1
     1 -> 0