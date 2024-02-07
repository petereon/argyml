{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (isPrefixOf)
import Data.Yaml (ToJSON, encode, object, toJSON, (.=))
import GHC.Generics
import System.Environment (getArgs)

data ArgLike = Option {key :: String, value :: String} | Flag {key :: String} | Arg {value :: String} deriving (Show, Generic)

instance ToJSON ArgLike where
  toJSON (Option key value) = object ["type" .= ("option" :: String), "key" .= key, "value" .= value]
  toJSON (Flag key) = object ["type" .= ("flag" :: String), "key" .= key]
  toJSON (Arg value) = object ["type" .= ("arg" :: String), "value" .= value]

toRepr :: ToJSON a => a -> String
toRepr val = BS.unpack (encode val)

data ArgStruct = ArgStruct
  { options :: [(String, String)],
    flags :: [String],
    args :: [String]
  }
  deriving (Show)

instance Semigroup ArgStruct where
  (<>) :: ArgStruct -> ArgStruct -> ArgStruct
  (<>) (ArgStruct options' flags' args') (ArgStruct options'' flags'' args'') =
    ArgStruct (options' <> options'') (flags' <> flags'') (args' <> args'')

instance Monoid ArgStruct where
  mempty :: ArgStruct
  mempty = ArgStruct [] [] []

parseArgLike :: [String] -> [ArgLike]
parseArgLike [] = []
parseArgLike (x : xs)
  | hasDashPrefix x = case xs of
      (y : ys) | not (hasDashPrefix y) -> Option x y : parseArgLike ys
      _ -> Flag x : parseArgLike xs
  | otherwise = Arg x : parseArgLike xs
  where
    hasDashPrefix = ("-" `isPrefixOf`)

main :: IO ()
main = putStrLn . toRepr . parseArgLike =<< getArgs
