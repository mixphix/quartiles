module Main where

import Data.Foldable
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text

allCombs :: (Eq a) => [a] -> [[a]]
allCombs xs =
  [1 .. 4] >>= \n -> filter ((n ==) . length . List.nub) $ mapM (const xs) [1 .. n]

quartiles :: [Text] -> [Text]
quartiles from = map fold $ allCombs from

main :: IO ()
main = do
  putStrLn "Enter a space-separated list of quartiles:"
  qs <- Text.words <$> Text.getLine
  knownWords <- Text.lines <$> Text.readFile "Top100000.txt"
  mapM_ Text.putStrLn $ filter (`elem` knownWords) (quartiles qs)
