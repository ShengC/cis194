module Lecture8.HW08 where

import Text.Read (readMaybe)
import Data.List (stripPrefix)

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
  where go :: String -> Maybe String
        go (x:xs) =
          ((readMaybe [x]) :: Maybe Int) >>= \n ->
          stripPrefix (replicate n 'a') xs >>= \ys ->
            case ys of
              [] -> Just []
              _  -> go ys
        isJust (Just []) = True
        isJust _         = False

specialNumbers :: [Int]
specialNumbers = [x | x <- [1..100], x `mod` 5 == 0, x `mod` 7 /= 0]
