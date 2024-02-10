{-# LANGUAGE DeriveGeneric #-}
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
