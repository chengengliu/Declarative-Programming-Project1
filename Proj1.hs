-- Chengeng Liu 
-- Student ID : 813174
-- Declarative Programming COMP30020
-- Start time : 20/8/2018

-- This is a guessing game. There are two culprits and the program should 
-- be able to catch the correct culprits as soon as possible by using the 
-- minimum guesses. Each person has three features, height(S/T), haircolor
-- (B/R/D) and sex (M/F). The 'feedback' function will output the score
-- for the guess. The 'nextGuess' will generate the next best guess based
-- on the last guess and the remaining possible guesses' performance. There 
-- are several helper functions for the 'nextGuess' function in order to get
-- a more precise guess. 

-- The data structure used in the program are 'Person', 'Guess', 'GameState'
-- and 'Score'. Person is simply a list of char(String). It is used to 
-- store each charactistic of the person. Guess is a list of Person ([Person])
-- where Guess is suppossed to include two Persons at a time. GameStae is a 
-- list of Guess ([Guess]), which should include the remaining possible 
-- guesses. 

------------------------------------------------------------------------------

module Proj1 (Person, parsePerson, height, hair, sex,
              GameState, initialGuess, nextGuess, feedback) where
import Data.List
import Data.Maybe
import Data.Function

type Person = String  -- [Char]
type Guess = [Person] -- [[Char]] == [String ]
type GameState = [Guess] -- [[[Char]]]  == [[Person]]
type Score = (Int, Int, Int, Int)
------------------------------------------------------------------------------
-- Helper Function.
-- Given a string and transform the string into a 'Person' 
parsePerson :: String -> Maybe Person 
parsePerson [] = Nothing
parsePerson (h:hc:s:[]) = Just [h,hc,s]

-- Given a Person and return the information about the person.
height :: Person -> Char
height (h:hc:s:[]) = h 
hair :: Person -> Char 
hair (h:hc:s:[]) = hc
sex :: Person -> Char 
sex (h:hc:s:[]) = s
------------------------------------------------------------------------------
-- Make an initial guess. Generate all possible guesses and store in a state
-- Return the first guess and the game state after selecting the first guess. 

initialGuess :: ([Person], GameState)
initialGuess = (initialGuess, state)
         where
          people = [fromJust (parsePerson (h ++ hc ++ s))
            | h <-["S", "T"],hc <-["B", "R", "D"],s  <-["M", "F"]]
          allStates = [[a,b]| a<-people, b<- people, a<b]     
          initialGuess = ["SBM", "TRM"]
          state = allStates \\ [initialGuess]
      
------------------------------------------------------------------------------
-- Return the score given two inputs(guesses). Basically describe the accuracy
-- of the guess. A total correct means that all height, hair and gender are 
-- the same. If there is a total correct, then it will not be considered 
-- as a partial correct. 

feedback :: Guess -> Guess -> Score
feedback culprits lineups = 
    (totalCorrect, correctHeight, correctColor, correctSex)
    where 
      guess = nub lineups 
      totalNumber = length guess
      totalCorrect = length $ intersect guess culprits 
      (height, hairColor, sex) = (0,1,2)
      -- The deleteFirstsBy function takes a predicate and two lists and 
      -- returns the first list with 
      -- the first occurrence of each element of the second list removed.
      correctHeight =  totalNumber - totalCorrect-
        length (deleteFirstsBy (areTheSame height) culprits lineups) 
      correctColor = totalNumber - totalCorrect-
        length (deleteFirstsBy (areTheSame hairColor) culprits lineups)
      correctSex = totalNumber - totalCorrect- 
        length (deleteFirstsBy (areTheSame sex) culprits lineups)
        
------------------------------------------------------------------------------
-- Select the next guess from the updated state, given the last guess and 
-- last score. 

nextGuess :: ([Person],GameState) -> Score -> ([Person],GameState)
nextGuess (lastGuess, state) score = (newGuess, newState)
  where 
    -- Cut the state such that only the guesses that 
    -- have the same score will be remained. 
    newState = delete lastGuess [candidate | candidate <- state,
      feedback candidate lastGuess == score] 
    newGuess  = selectGuess newState 

------------------------------------------------------------------------------
-- Compute the average number of possible lineups that will remain 
-- after each lineup.For each possible guesses in the remaining state, 
-- calculate the utility score for the guess. Then based on the 
-- effeciency performance of each guess, select the most effect 
-- one ( with the most minimum possible candidates remaining. )

selectGuess :: GameState -> Guess 
selectGuess state = bestGuess -- The first one is the most effective. 
  where 
    candidates = [(guess, utilityScore)
      | guess <- state, 
      let utilityScore = utility guess state]
    guesses = sortBy(compare `on` snd ) candidates 
    bestGuess = fst (head guesses)
    -- Sort all remaining choices based on the effeciency. 

------------------------------------------------------------------------------
-- Calculate the expected number of possible guesses will remain after the 
-- guess that you made. Group the result that have the same score and divided
-- by the total possibilities times the group number. This is the efficiency 
-- score for one guess. Calculate the utility score for each guess and sort
-- them. The fewer score it has, the more efficient the guess is. 

utility :: Guess -> GameState -> Double
utility lastGuess state =  sum [(numPos / total) * numPos| g<- groupedScores, 
  let numPos = (fromIntegral .length) g]
  where 
    totalScores = [score | guess <- state, 
      let score = feedback lastGuess guess]
    total = (fromIntegral . length ) state
    groupedScores = (group . sort ) totalScores 

 
------------------------------------------------------------------------------
-- Given two lists and the postion we are interested in, return true
-- If and only if the nth element in two lists are the same. 
  
areTheSame :: Int -> Person -> Person -> Bool
areTheSame n first second  =  (first !! n) == (second !! n )

