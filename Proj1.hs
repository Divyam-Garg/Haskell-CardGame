--Author: Divyam Garg
{-Purpose: Implement part of the functionality required to play the game
           defined in spec for proj1 of Declarative Programming -}

{- The game has an answerer and a guesser. Answerer chooses a number of cards
   at random. The guesser has to guess this set of cards. For each guess the 
   guesser gets certain feedback allowing it to refine its guesses. The process
   stops when the guesser gets the right answer. The aim is to get the right
   answer in the least number of guesses. The functionalities implemented in
   this file are functions for feedback to a certain guess, providing an 
   initial guess and providing a refined next guess after processing the 
   feedback for the last guess. GameState stores a list of possible answers. 
   The strategy for initial guess is to equally distance the cards in order to
   eliminate most possible answers on average. For nextGuess, we compute a 
   score for each possible answer in gameState by running every answer as the 
   guess against every answer in the gameState and then taking the weighted 
   average of the frequency of every feedback, the lower the score, the more 
   spread out the answers are amongst all possible feedbacks and so chances for
   most eliminations are higher.-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Card
import Data.Map

-- TYPE SYNONYMS --------------------------------------------------------------

--GameState stores the list of all the possible answers. After every guess
--inconsistent answers are removed from the GameState
type GameState = [Answer]

--Guess and Answer are both lists of cards. Defining both Guess and Answer makes
--the type declarations much less cryptic and more informational.
type Guess = [Card]
type Answer = [Card]

--Feedback is the tuple of 5 ints returned by the feedback function.
type Feedback = (Int, Int, Int, Int, Int)

THRESHOLD = 1000

-------------------------------------------------------------------------------

--PUBLIC FUNCTIONS ------------------------------------------------------------

--Takes in the answer and guess cards and returns feedback as to how close
--guess was to answer
feedback :: Answer -> Guess -> Feedback
feedback as gs = (noCorrectCard as gs, noLowerRank as gs, noCorrectRank as gs,
                                      noHigherRank as gs, noCorrectSuit as gs)


--Returns the list of cards forming initial guess and the gamestate.
-- Only works for games of upto 4 cards
initialGuess :: Int -> (Guess, GameState)

--choose over the range of all cards gives all possible ways of selecting n
--cards from a deck in a list. This forms the list of possible answers
initialGuess n = (initialGuessCards n, choose ([minBound..maxBound]::[Card]) n)


--Returns the next guess by guesser and the gameState after processing the 
--feedback for the last guess
nextGuess :: (Guess, GameState) -> Feedback -> (Guess, GameState)
nextGuess (g,gameState) fb = (bestGuess newGs, newGs)

    --filter the possible answers that are inconsistent with the last guess
    where newGs = [ a | a <- gameState, (feedback a g) == fb] 

------------------------------------------------------------------------------

-- HELPER FUNCTIONS FOR FEEDBACK ----------------------------------------------

--Returns the number of correct guesses
noCorrectCard :: Answer -> Guess -> Int
noCorrectCard [] gs = 0
noCorrectCard (a:as) gs = boolToInt(elem a gs) + noCorrectCard as gs


{-Returns the number of cards in answer that have a rank lower than the lowest
    rank card in guess -}
noLowerRank :: Answer -> Guess -> Int
noLowerRank as gs = length [(Card s r) | (Card s r)<-as, (r < lowestRank gs)]


--Returns the number of cards in guess with the correct rank
-- \\ is the set difference operator
noCorrectRank :: Answer -> Guess -> Int
noCorrectRank as gs = length as - length (deleteFirstsBy eqRank as gs)


--Returns the number of cards in answer that have a rank higher than the
--highest rank card in guess 
noHigherRank :: Answer -> Guess -> Int
noHigherRank as gs = length [(Card s r) | (Card s r)<-as, (r > highestRank gs)]


--Returns the number of cards in guess with the correct suit
noCorrectSuit :: Answer -> Guess -> Int
noCorrectSuit as gs = length as - length (deleteFirstsBy eqSuit as gs)


--Returns true if the cards have the same rank
eqRank :: Card -> Card -> Bool
eqRank (Card s1 r1) (Card s2 r2) = r1 == r2


--Returns true if the cards have the same rank
eqSuit :: Card -> Card -> Bool
eqSuit (Card s1 r1) (Card s2 r2) = s1 == s2


--Returns the rank of the card with the lowest rank in the list
lowestRank :: [Card] -> Rank

lowestRank [Card s r] = r
lowestRank (Card s r:cards)
    | r < lowestSoFar = r
    | otherwise = lowestSoFar
    where lowestSoFar = lowestRank cards 


--Returns the rank of the card with the highest rank in the list
highestRank :: [Card] -> Rank

highestRank [Card s r] = r
highestRank (Card s r:cards)
    | r > highestSoFar = r
    | otherwise = highestSoFar
    where highestSoFar = highestRank cards 


--Returns the integer representation of boolean
boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

-------------------------------------------------------------------------------

-- HELPER FUNCTIONS FOR INITIALGUESS ------------------------------------------

--Returns the list of cards forming the initial guess
initialGuessCards :: Int -> Guess
initialGuessCards n = [ (Card s r) | (s,r) <- zip ([minBound..maxBound]::[Suit]) 
                                                        (initialGuessRanks n) ]


--Returns a list of equally spaced card ranks to use in initial guess
initialGuessRanks :: Int -> [Rank]
initialGuessRanks n = Data.List.take n (enumFromThenTo (toEnum (interval)) 
                                (toEnum (2*interval+1)) (toEnum 12) :: [Rank])
    where interval = div 13 (n+1) --apt distance between 2 cards in the guess


--Returns all possible combinations of a given size
choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose [] _ = []

--All the ways to choose n things from a list is the way to choose n things
--excluding the first and the first thing added to all the ways to choose 
--(n-1) things excluding the first.
choose (c:cs) n = (choose cs n) ++ [ c:a | a <- choose cs (n-1)]

-------------------------------------------------------------------------------

-- HELPER FUNCTIONS FOR NEXTGUESS ---------------------------------------------

--Returns the best guess out of all the possible answers in GameState
bestGuess :: GameState -> Guess

--pick the guess with the least score
--if theres too many possible answers in gameState, guess the middle item to
--save on runtime
bestGuess gameState

    --gameState too long, guess randomly, calculating scores too expensive
    | lenGameState > THRESHOLD = gameState !! (div lenGameState 2)

    --calculate feedbacks and get the least
    | otherwise = fst (getLeastScoreGuess (scoreGuesses guessFbFreqMaps))

    --run every possible guess against every possible answer and store the 
    --feedbacks
    where fbs = [(g, [(feedback g a, 1) | a <- gameState]) | g <- gameState]

          --form a feedbackFreqMap for each guess, used to compute its score
          guessFbFreqMaps = [(g, fromListWith (+) fbList) | (g,fbList) <- fbs]
          lenGameState = length gameState


--Returns a list of tuples where the fst is the guess and snd is its score
scoreGuesses :: [(Guess, Map Feedback Int)] -> [(Guess, Int)]
scoreGuesses guessMaps = [ (g,scoreFbFreqMap map) | (g,map) <- guessMaps]


--Calculates and Returns the score of a particular guess from its (feedback, 
--answerList) map. Score is defined as the weighted average of the number of
--answers that produce a feedback for all feedbacks for a given guess
scoreFbFreqMap :: Map Feedback Int -> Int
scoreFbFreqMap fbFreqMap = div sumSqLen sumLen
    where sumSqLen = sum (Data.List.map (^2) groupedAnsList)
          sumLen =  sum groupedAnsList

          --get the list of freq values from map
          groupedAnsList = [ fbFreqMap ! key | key <- (keys fbFreqMap)]


--Returns the guess and its score with the least score
getLeastScoreGuess :: [(Guess, Int)] -> (Guess, Int)
getLeastScoreGuess [a] = a 
getLeastScoreGuess (guessScore: guessScores)
    | snd guessScore < snd leastSoFar = guessScore
    | otherwise = leastSoFar
    where leastSoFar = (getLeastScoreGuess guessScores)

-------------------------------------------------------------------------------