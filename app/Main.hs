module Main where
import System.Environment (getArgs)
import Data.List

data ArgLike = Option String String | Flag String | Arg String deriving (Show)

main :: IO ()
main = do
     stdIn <- getContents
     args <- getArgs
     print $ parseArgLike args
     print stdIn

parseArgLike :: [String] -> [ArgLike]
parseArgLike [] = []
parseArgLike [x]
    | "-" `isPrefixOf` x = [Flag x]
    | otherwise = [Arg x]

parseArgLike (x:y:xs) 
    | "-" `isPrefixOf` x && "-" `isPrefixOf` y = Flag x : parseArgLike (y : xs)
    | "-" `isPrefixOf` x = Option x y : parseArgLike xs
    | otherwise = Arg x : parseArgLike (y : xs)