module Main where

import Data.Foldable (Foldable (fold))
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Traversable (for)

quartiles :: [Text] -> [Text]
quartiles from = map fold do
  n <- [1 .. 4]
  filter ((n ==) . length . List.nub) do
    for [1 .. n] (const from)

main :: IO ()
main = do
  putStrLn "Enter a space-separated list of quartiles:"
  qs <- Text.words <$> Text.getLine
  knownWords <- Set.fromAscList . Text.lines <$> Text.readFile "Top100000.txt"
  let results = List.sort $ filter (`Set.member` knownWords) (quartiles qs)
  putStrLn $ show (length results) ++ " results"
  mapM_ Text.putStrLn results
