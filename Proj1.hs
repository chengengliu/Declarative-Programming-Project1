--module Proj1 (Person, parsePerson, height, hair, sex,
  --            GameState, initialGuess, nextGuess, feedback) where
import Data.List
import Data.Maybe
import Data.Function

type Height = Char
type HairColor = Char
type Sex = Char
type Person = String  -- [Char]
type Guess = [Person] -- [[Char]] == [String ]
type GameState = [Guess] -- [[[Char]]]   GameState Should be [[Person]] (from Zijun)
type Score = (Int, Int, Int, Int)
{-
takes a three-character string and returns Just p, where p is the person specified by that string. 
If an invalid string is provided, returns Nothing.
-}
parsePerson :: String -> Maybe Person 
parsePerson [] = Nothing
parsePerson (h:hc:s:[]) = Just [h,hc,s]
height :: Person -> Char
height (h:hc:s:[]) = h 
hair :: Person -> Char 
hair (h:hc:s:[]) = hc
sex :: Person -> Char 
sex (h:hc:s:[]) = s

initialGuess :: ([Person], GameState)
initialGuess = (guess, state)
         where     
          allPeople= [ [h,hc,s] | h<-['S','T'], hc<- ['B','R','D'], s<-['M','F'] ] 
          guess = ["SBM"]
          state = [allPeople \\ guess]
      
-- Ideally this function should return the score given two inputs, one is the culprits and the other
-- is the lineup.
feedback :: Guess -> Guess -> (Int, Int , Int, Int)
feedback culprits lineups = 
    (totalCorrect, correctHeight, correctColor, correctSex)
    where 
      guess = nub lineups 
      totalNumber = length guess
      totalCorrect = length $ intersect guess culprits 

      (height, hairColor, sex) = (0,1,2)
      -- The deleteFirstsBy function takes a predicate and two lists and returns the first list with 
      -- the first occurrence of each element of the second list removed.
      correctHeight =  totalNumber -
        length (deleteFirstsBy (areTheSame height) culprits lineups)-
        totalCorrect
      correctColor = totalNumber -
        length (deleteFirstsBy (areTheSame hairColor) culprits lineups)-
        totalCorrect
      correctSex = totalNumber - 
        length (deleteFirstsBy (areTheSame sex) culprits lineups)-
        totalCorrect


        --[c | c1<- culprits, l1 <- lineups, let c =areTheSame height c1 l1]

-- Given two lists and the postion we are interested in, return true
-- If and only if the nth element in two lists are the same. 
  -- Helper function 
  --Add a function reused for calculating the final score. 
areTheSame :: Int -> Person -> Person -> Bool
areTheSame n first second  =  (first !! n) == (second !! n )


-- Person:: [Char]
-- [Person] :: [[Char]]
-- GameState :: [Person]  == [[Char]]
nextGuess :: ([Person],GameState) -> (Int,Int,Int,Int) -> ([Person],GameState)
nextGuess (lastGuess, state) score = (head newGuess, newState)
  


selectGuess :: GameState -> Guess 
selectGuess state = fst (head guess)
  where 
    candidates = [(goal, candidate)
    | goal <- state, 
    let currentState = state \\ [goal],
    let candidate = utilityState goal currentState
    ]
    guess = 

  


guessJudge :: Eq a => (a->a) -> [a] -> [a] -> Int -> Bool
guessJudge f x y n = length(xs \\ ys) == 2 -n
    where 
      xs = map f x
      ys = map f y 


-- Get a utility function to judge the effectness of a guess by calculating
-- the remaining percentage of possible guesses. 
-- The samll number is, the better efficiency it is. 

utilityState ::  Guess -> GameState -> Double
utilityState culprits states = 
  sum [ nt/ totalCombinations | g <-individuals, let nt = (fromIntegral . length) g]
  where
    scores =  [score | guess <- states, let score = feedback culprits guess]
    totalCombinations = (fromIntegral . length) scores
    individuals = (sort.group) scores


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
    print (feedback c1 l1 ) 
    --print (c1, l1)
    print $ feedback c2 l2
    --print (c2, l2)
    print $ feedback c3 l3
    --print (c3, l3)
    print $ feedback c4 l4
    --print (c4, l4)