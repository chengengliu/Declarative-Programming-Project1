module Proj1 (Person, parsePerson, height, hair, sex,
              GameState, initialGuess, nextGuess, feedback) where
import Data.List
import Data.Maybe
import Data.Function

type Height = Char
type HairColor = Char
type Sex = Char
type Person = String  -- [Char]
type Guess = [Person] -- [[Char]] == [String ]
type GameState = [Guess] -- [[[Char]]]  == [[Person]]
type Score = (Int, Int, Int, Int)
----------------------------------------------------------------------------------------------
-- Helper Function 
parsePerson :: String -> Maybe Person 
parsePerson [] = Nothing
parsePerson (h:hc:s:[]) = Just [h,hc,s]
height :: Person -> Char
height (h:hc:s:[]) = h 
hair :: Person -> Char 
hair (h:hc:s:[]) = hc
sex :: Person -> Char 
sex (h:hc:s:[]) = s
----------------------------------------------------------------------------------------------
-- Make an initial guess, hardcode for the first guess as there is not enough information about
-- it.
initialGuess :: ([Person], GameState)
initialGuess = (initialGuess, state)
         where     
          combinations= [ [h,hc,s] | h<-['S','T'], hc<- ['B','R','D'], s<-['M','F'] ]
          allPeople = [guess | guess <- subsequences combinations, length guess == 2] 
          initialGuess = ["SBM", "TRF"]
          state = allPeople \\ [initialGuess]
      
          
-- Return the score given two inputs, one is the culprits and the other
-- is the lineup.

feedback :: Guess -> Guess -> Score
feedback culprits lineups = 
    (totalCorrect, correctHeight, correctColor, correctSex)
    where 
      guess = nub lineups 
      totalNumber = length guess
      totalCorrect = length $ intersect guess culprits 
      (height, hairColor, sex) = (0,1,2)
      -- The deleteFirstsBy function takes a predicate and two lists and returns the first list with 
      -- the first occurrence of each element of the second list removed.
      correctHeight =  totalNumber - totalCorrect-
        length (deleteFirstsBy (areTheSame height) culprits lineups) 
      correctColor = totalNumber - totalCorrect-
        length (deleteFirstsBy (areTheSame hairColor) culprits lineups)
      correctSex = totalNumber - totalCorrect- 
        length (deleteFirstsBy (areTheSame sex) culprits lineups)
        

-- Given two lists and the postion we are interested in, return true
-- If and only if the nth element in two lists are the same. 
  -- Helper function 
  --Add a function reused for calculating the final score. 
areTheSame :: Int -> Person -> Person -> Bool
areTheSame n first second  =  (first !! n) == (second !! n )


-- Person:: [Char]
-- [Person] :: [[Char]]
-- GameState :: [Person]  == [[Char]]

bestGuess :: Guess -> GameState -> Guess
bestGuess lastGuess state = bestguess
  where 
    scoreList = [(guess, utilityScore)| guess <- state, 
      let utilityScore = utility lastGuess state ]
    guesses = sortBy (compare `on` snd)  scoreList
    bestguess = fst $ head guesses


utility :: Guess -> GameState -> Double
utility lastGuess state =  sum [(numPos / total) * numPos| g<- grouped, let numPos = (fromIntegral .length) g]
  where 
    totalScores = [score | guess <- state, let score = feedback lastGuess guess]
    total = (fromIntegral . length ) state
    grouped = (group . sort ) totalScores 


nextGuess :: ([Person],GameState) -> Score -> ([Person],GameState)
nextGuess (lastGuess, state) score = (newGuess, newState)
  where 
    -- Cut the state such that only the guesses that 
    -- have the same score will be remained. 
    newState = delete lastGuess [candidate | candidate <- state,
      feedback candidate lastGuess == score] 
    newGuess  = selectGuess newState 
      -- selectGuess newState

-- Compute the average number of possible lineups that will remain 
--after each lineup.For each possible guesses in the remaining state, 
-- calculate the utility score for the guess. Then based on the 
--effeciency performance of each guess, select the most effect 
--one ( with the most minimum possible candidates remaining. )
selectGuess :: GameState -> Guess 
selectGuess state = fst (head guesses) -- The first one is the most effective. 
  where 
    candidates = [(guess, utilityScore)
      | guess <- state, 
      let currentState = state \\ [guess],
      let utilityScore = utilityCal guess currentState]
    guesses = sortBy(compare `on` snd ) candidates 
    -- Sort all remaining choices based on the effeciency. 


-- Simply compute expected number of remaining possible lineups for each guess
-- by grouping guesses with the same feedback(scores).
-- The smaller the number is, the more efficient the reduction is. 
-- totslScores is [Score] after sorting and grouping, the number of the 
-- same score will be calculated. 
utilityCal ::  Guess -> GameState -> Double 
utilityCal possibleSelection states = 
  sum [ (eachPossNumber/ totalCombinations)*eachPossNumber
    | g <-groupLineups, let eachPossNumber = (fromIntegral . length) g]
  where
    totalScores =  [score | guess <- states, 
      let score = feedback possibleSelection guess]
    totalCombinations = (fromIntegral . length) states
    groupLineups = (group.sort) totalScores   




main = do 
    let person = (parsePerson "ABC")
    --let (people, gamestate, allPeople) = initialGuess
    --print (1)
    --print (parsePerson "abc")
    --print (hair (fromJust person))
    print (initialGuess )


    -- Unit test for feedback  
    let c1 = ["SBM", "SBF"]
        l1 = ["SBM", "TRF"]
        c2 = ["SBM", "SBF"]
        l2 = ["TRF", "SBM"]
        c3 = ["SBM", "SBF"]
        l3 = ["TRF", "TRM"]
        c4 = ["SBM", "TDF"]
        l4 = ["TRF", "SDM"]
    print (parsePerson "abc")
    print (feedback c1 l1 ) 
    --print (c1, l1)
    print $ feedback c2 l2
    --print (c2, l2)
    print $ feedback c3 l3
    --print (c3, l3)
    print $ feedback c4 l4
    --print (c4, l4)