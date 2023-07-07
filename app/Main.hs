{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.List ( isPrefixOf )
import System.Environment (getArgs)
data ArgLike = Option String String | Flag String | Arg String deriving (Show)

data ArgStruct = ArgStruct
  { options :: [(String, String)],
    flags :: [String],
    args :: [String]
  } deriving (Show)

instance Semigroup ArgStruct where
  (<>) :: ArgStruct -> ArgStruct -> ArgStruct
  (<>) (ArgStruct options' flags' args') (ArgStruct options'' flags'' args'') =
    ArgStruct (options' <> options'') (flags' <> flags'') (args' <> args'')

instance Monoid ArgStruct where
  mempty :: ArgStruct
  mempty = ArgStruct [] [] []

prettyFormat :: ArgStruct -> String
prettyFormat (ArgStruct options' flags' args') =
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

argToStruct :: ArgLike -> ArgStruct
argToStruct (Option key value) = ArgStruct [(key, value)] [] []
argToStruct (Flag flag) = ArgStruct [] [flag] []
argToStruct (Arg arg) = ArgStruct [] [] [arg]

restructureArgLike :: [ArgLike] -> ArgStruct
restructureArgLike = foldMap argToStruct

main :: IO ()
main = putStrLn . prettyFormat . restructureArgLike . parseArgLike =<< getArgs
