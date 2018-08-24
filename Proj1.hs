--module Proj1 (Person, parsePerson, height, hair, sex,
  --            GameState, initialGuess, nextGuess, feedback) where
import Data.List
import Data.Maybe
import Debug.Trace

type Height = Char
type HairColor = Char
type Sex = Char
type Person = [String]
type State = [Person]

type Score = (Int, Int, Int, Int)

data GameState = GameState [Person]
        deriving (Show,Eq,Ord)


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


initialGuess :: ([Person], State)
initialGuess = (guess, state)
         where     
          allPeople= [ [h,hc,s] | h<-['S','T'], hc<- ['B','R','D'], s<-['M','F'] ] 
          --  如果是 allPeople = Person ([[h,hc,s]| h<-['S','T'], hc<- ['B','R','D'], s<-['M','F']])
          --  这样写就不行。很奇怪。。没法消除掉这个guess
          guess = [["S","B","M"]]
            --["SBM"]
          state =allPeople \\ guess
          -- allPeople :: Person 
            {-allPeople = Person ([[h,hc,s]| h<-['S','T'], hc<- ['B','R','D'], s<-['M','F']])
            onePossible = Person ["SBM"]
            guess = [OnePossible]    -- guess :: [Person]
            state = GameState ([allPeople] \\ [Person ["SBF"]])     -- GameState needs a list of Person. -}
            

main = do 
    let person = (parsePerson "ABC")
    --let (people, gamestate, allPeople) = initialGuess
    print (1)
    print (parsePerson "abc")
    print (hair (fromJust person))
    print (initialGuess )