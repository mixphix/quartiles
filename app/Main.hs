module Main where

import Data.Foldable (Foldable (fold))
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

-- length (tiles 3 [1 .. n]) == n^3 + n^2 + n + 1
tiles :: (Eq a) => Int -> [a] -> [[a]]
tiles m from = do
  n <- [1 .. m]
  filter ((n ==) . length . List.nub) do
    traverse (const from) [1 .. n]

main :: IO ()
main = do
  putStrLn "Enter a space-separated list of quartiles:"
  qs <- Text.words <$> Text.getLine
  knownWords <- Set.fromAscList . Text.lines <$> Text.readFile "words.txt"
  let results = List.sort $ filter (`Set.member` knownWords) (map fold $ tiles 4 qs)
  putStrLn $ show (length results) ++ " results"
  mapM_ Text.putStrLn results
