--module Proj1 (Person, parsePerson, height, hair, sex,
  --            GameState, initialGuess, nextGuess, feedback) where
import Data.List
import Data.Maybe
import Debug.Trace

type Height = Char
type HairColor = Char
type Sex = Char
type Person = String  -- Deceide to treat Person as a list of char[Char]. Rather than list of list of char.([String])
type GameState = [Person]

type Score = (Int, Int, Int, Int)

--data GameState = GameState [Person]
--        deriving (Show,Eq,Ord)

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
          state = allPeople \\ guess

          -- 夜访吸血鬼
          -- allPeople :: Person 
            {-allPeople = Person ([[h,hc,s]| h<-['S','T'], hc<- ['B','R','D'], s<-['M','F']])
            onePossible = Person ["SBM"]
            guess = [OnePossible]    -- guess :: [Person]
            state = GameState ([allPeople] \\ [Person ["SBF"]])     -- GameState needs a list of Person. -}
          
-- Ideally this function should return the score given two inputs, one is the culprits and the other
-- is the lineup.
feedback :: [Person] -> [Person] -> (Int, Int , Int, Int)
feedback culprits lineups = 
    (totalCorrect, correctHeight, correctColor, correctSex)
    where 
      guess = nub lineups 
      totalNumber = length guess
      totalCorrect = length $ intersect guess culprits 

      (height, hairColor, sex) = (0,1,2)
      --The deleteFirstsBy function takes a predicate and two lists and returns the first list with the first occurrence of each element of the second list removed.
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
-- 我知道了 问题出在了对 areTheSame的这里。 判断有问题。
-- input的type是 [[[Char]]], 也就是list of list of string，三层的char。这里areTheSame比较的
-- 是以为个人为单位的相同。而我们这里需要的是以字母为单位的比较的函数。 
nextGuess :: ([Person], GameState) -> Score -> ([Person], GameState)
nextGuess (last, state) score = (next, newState)
  where 
    newState = delete last [ p | p<- state, feedback [p] last == score ]
    next = bestGuess
        where 
          bestGuess = take 1 $ head $ sortBy(compare `on` snd ) afterDele
          afterDele = [(culprits, lineups) |
            culprits <- newState, 
            let deleteState = newState \\ [culprits],
            let lineups = utilityState culprits deleteState]
      
-- Get a utility function to judge the effectness of a guess by calculating
-- the remaining percentage of possible guesses. 
-- The samll number is, the better efficiency it is. 

utilityState ::  [Person] -> GameState -> Double
utilityState culprits states = 
  sum [ nt/ totalCombinations | g <-individuals, let nt = (fromIntegral . length) g]
  where
    scores =  [score | guess <- states, let score = feedback culprits  [guess]]
    totalCombinations = (fromIntegral . length) scores
    individuals = (sort.group) scores


main = do 
    let person = (parsePerson "ABC")
    --let (people, gamestate, allPeople) = initialGuess
    --print (1)
    --print (parsePerson "abc")
    --print (hair (fromJust person))
    --print (initialGuess )


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