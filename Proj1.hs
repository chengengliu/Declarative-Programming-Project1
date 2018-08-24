--module Proj1 (Person, parsePerson, height, hair, sex,
  --            GameState, initialGuess, nextGuess, feedback) where
import Data.List
import Data.Maybe
import Debug.Trace

type Height = Char
type HairColor = Char
type Sex = Char
type Person = [String]
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
parsePerson (h:hc:s:[]) = Just [[h,hc,s]]


height :: Person -> Char
height ([h:hc:s:[]]) = h 

hair :: Person -> Char 
hair ([h:hc:s:[]]) = hc

sex :: Person -> Char 
sex ([h:hc:s:[]]) = s


initialGuess :: ([Person], GameState)
initialGuess = ([guess], state)
         where     
          allPeople= [ [h,hc,s] | h<-['S','T'], hc<- ['B','R','D'], s<-['M','F'] ] 
          guess = ["SBM"]
          state = [allPeople \\ guess]

          -- 夜访吸血鬼
          -- allPeople :: Person 
            {-allPeople = Person ([[h,hc,s]| h<-['S','T'], hc<- ['B','R','D'], s<-['M','F']])
            onePossible = Person ["SBM"]
            guess = [OnePossible]    -- guess :: [Person]
            state = GameState ([allPeople] \\ [Person ["SBF"]])     -- GameState needs a list of Person. -}
          

--nextguess :: ([Person], GameState) -> (Int,Int,Int,Int) -> ([Person], GameState)



-- Ideally this function should return the score given two inputs, one is the culprits and the other
-- is the lineup.
feedback :: [Person] -> [Person] -> (Int, Int, Int, Int, Char )
feedback culprits lineups = 
    (totalCorrect, correctHeight, correctColor, correctSex, height')
    where 
      guess = nub lineups 
      totalCorrect = length $ intersect guess culprits 
      -- ** I need to figure our a way to calculate the correct numebrs. 
      -- ** The problem here is that how to determine the correct three things. 

      height' = height $ culprits !! 0 
      --heightComparison = 
      correctHeight = 1
      correctColor = 1
      correctSex = 1
  
areTheSame :: Ord a => [a] -> [a] -> Int -> Bool
areTheSame first second n =  (first !! n ) == (second !! n)


main = do 
    let person = (parsePerson "ABC")
    --let (people, gamestate, allPeople) = initialGuess
    --print (1)
    --print (parsePerson "abc")
    --print (hair (fromJust person))
    --print (initialGuess )


    -- Unit test for feedback  
    let c1 = [["SBM"], ["SBF"]] 
        l1 = [["SBM"], ["TRF"]]
        c2 = [["SBM"], ["SBF"]]
        l2 = [["TRF"], ["SBF"]]
        c3 = [["SBM"], ["SBF"]]
        l3 = [["TRF"], ["TRM"]]
    print (feedback c1 l1)
    print $ feedback c3 l3