module Main where

import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

tiles :: (Ord a) => Int -> [a] -> [[a]]
tiles m from = do
  n <- [1 .. m]
  filter ((n ==) . Set.size . Set.fromList) $ traverse (const from) [1 .. n]

main :: IO ()
main = do
  putStrLn "Enter a space-separated list of quartiles:"
  qs <- Text.words <$> Text.getLine
  knownWords <- Set.fromDistinctAscList . Text.lines <$> Text.readFile "words.txt"
  let results = List.sort $ filter (`Set.member` knownWords) (Text.concat <$> tiles 4 qs)
  putStrLn $ show (length results) ++ " results"
  mapM_ Text.putStrLn results
