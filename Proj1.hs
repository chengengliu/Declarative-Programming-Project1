--module Proj1 (Person, parsePerson, height, hair, sex,
  --            GameState, initialGuess, nextGuess, feedback) where
import Data.List
import Data.Maybe


type Height = Char
type HairColor = Char
type Sex = Char
data Person = Person [String]
        deriving Show

type Score = (Int, Int, Int, Int)

data GameState = GameState [Person]


main = do 
    let person = (parsePerson "ABC")
    print (1)
    print (parsePerson "abc")
    print (hair (fromJust person))

{-
takes a three-character string and returns Just p, where p is the person specified by that string. 
If an invalid string is provided, returns Nothing.
-}
parsePerson :: String -> Maybe Person 
parsePerson [] = Nothing
parsePerson (h:hc:s:[]) = Just (Person [h:hc:s:[]])


height :: Person -> Char
height (Person [h:hc:s:[]]) = h 

hair :: Person -> Char 
hair (Person [h:hc:s:[]]) = hc

sex :: Person -> Char 
sex (Person [h:hc:s:[]]) = s

{-
initialGuess :: ([Person], GameState)
initialGuess = (guess, state)
         where 
            
            guess = Person "SBM"-}
              
