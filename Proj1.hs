--module Proj1 (Person, parsePerson, height, hair, sex,
  --            GameState, initialGuess, nextGuess, feedback) where
import Data.List

type Height = Char
type HairColor = Char
type Sex = Char
data Person = Person [String]
        deriving Show

type Score = (Int, Int, Int, Int)

data GameState = GameState [Person]


main = do 
    print (1)
    print (parsePerson "abc")

parsePerson :: String -> Maybe Person 
parsePerson [] = Nothing
parsePerson (h:hc:s:[]) = Just (Person [h:hc:s:[]])

{-height :: Person -> Char
height (h:hc:s:[]) = h-}

{-
initialGuess :: ([Person], GameState)
initialGuess = (guess, state)
         where 
            
            guess = Person "SBM"-}
              
