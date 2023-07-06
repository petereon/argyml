module Main where

import Data.List
import System.Environment (getArgs)

data ArgLike = Option String String | Flag String | Arg String deriving (Show)

data ArgStruct = ArgStruct
  { options :: [(String, String)],
    flags :: [String],
    args :: [String]
  }

instance Show ArgStruct where
  show (ArgStruct options' flags' args') =
    unlines $
      ["options:"]
      -- TODO: use a better way to indent value, this is ugly
      -- This function needs a refactor in general
        ++ indentLines 2 (map (\(key, value) -> "- key: " ++ quote key ++ "\n    value: " ++ quote value) options')
        ++ ["flags:"]
        ++ indentLines 2 (map (\flag -> "- " ++ quote flag) flags')
        ++ ["arguments:"]
        ++ indentLines 2 (map (\arg -> "- " ++ quote arg) args')

indentLines :: Int -> [String] -> [String]
indentLines indentSize = map (replicate indentSize ' ' ++)

quote :: String -> String
quote s = "\"" ++ s ++ "\""

parseArgLike :: [String] -> [ArgLike]
parseArgLike [] = []
parseArgLike [x]
  | "-" `isPrefixOf` x = [Flag x]
  | otherwise = [Arg x]
parseArgLike (x : y : xs)
  | "-" `isPrefixOf` x && "-" `isPrefixOf` y = Flag x : parseArgLike (y : xs)
  | "-" `isPrefixOf` x = Option x y : parseArgLike xs
  | otherwise = Arg x : parseArgLike (y : xs)

restructureArgLike :: [ArgLike] -> ArgStruct
restructureArgLike [] = ArgStruct [] [] []
restructureArgLike (x : xs) = case x of
  Option a b -> ArgStruct ((a, b) : options rest) (flags rest) (args rest)
  Flag a -> ArgStruct (options rest) (a : flags rest) (args rest)
  Arg a -> ArgStruct (options rest) (flags rest) (a : args rest)
  where
    rest = restructureArgLike xs

main :: IO ()
main = do
  arguments <- getArgs
  print $ restructureArgLike $ parseArgLike arguments
