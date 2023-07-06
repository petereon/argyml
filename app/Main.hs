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
        ++ indentLines 2 ((>>=) options' (\(key, value) -> ["- key: " ++ quote key, "  value: " ++ quote value]))
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
parseArgLike (x : xs)
  | hasDashPrefix x = case xs of
      (y : ys) | not (hasDashPrefix y) -> Option x y : parseArgLike ys
      _ -> Flag x : parseArgLike xs
  | otherwise = Arg x : parseArgLike xs
  where
    hasDashPrefix = ("-" `isPrefixOf`)

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
