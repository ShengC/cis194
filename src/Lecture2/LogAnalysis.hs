module Lecture2.LogAnalysis where

import Lecture2.Log

import Data.Char (isDigit, ord)
import GHC.Exts (sortWith)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  map getContent . sortWith getTimestamp . filter (errorLevel 50) . inOrder . build . filter isError

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _                          = False

errorLevel :: Int -> LogMessage -> Bool
errorLevel n (LogMessage (Error n') _ _)
  | n <= n'   = True
  | otherwise = False
errorLevel _ _ = False

getTimestamp (LogMessage _ t _) = t
getContent (LogMessage _ _ s) = s

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ [root] ++ (inOrder right)

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg@(LogMessage _ t _) tree =
  case tree of
    Leaf -> Node Leaf msg Leaf
    Node left root@(LogMessage _ t' _) right ->
      if t <= t'
         then Node (insert msg left) root right
         else Node left root (insert msg right)

parseMessage' :: String -> LogMessage
parseMessage' xs =
  let ws = words xs
  in
    case ws of
      ("I":y:ys)   ->
        if all isDigit y
           then LogMessage Info (read y :: Int) (unwords ys)
           else Unknown xs
      ("W":y:ys)   ->
        if all isDigit y
           then LogMessage Warning (read y :: Int) (unwords ys)
           else Unknown xs
      ("E":y:z:zs) ->
        if (all isDigit y) && (all isDigit z)
           then LogMessage (Error (read y :: Int)) (read z :: Int) (unwords zs)
           else Unknown xs
      _            ->
        Unknown xs


parseMessage :: String -> LogMessage
parseMessage xs =
  case (parseMessageLevel . untilNonWhiteSpace $ xs) of
    Nothing -> Unknown xs
    Just (y, ys) ->
      case (parseInteger . untilNonWhiteSpace $ ys) of
        Just (z, zs) -> LogMessage y z (untilNonWhiteSpace zs)
        Nothing      -> Unknown xs


untilNonWhiteSpace :: String -> String
untilNonWhiteSpace = dropWhile (== ' ')

parseMessageLevel :: String -> Maybe (MessageType, String)
parseMessageLevel xs =
  case xs of
    'I' : ys -> Just (Info, ys)
    'W' : ys -> Just (Warning, ys)
    'E' : ys ->
      case (parseInteger . untilNonWhiteSpace $ ys) of
        Just (z, zs) -> Just (Error z, zs)
        Nothing      -> Nothing
    _        -> Nothing

parseInteger :: String -> Maybe (Int, String)
parseInteger xs =
  let (i, s) = parseInteger' 0 xs
  in
    if i == 0
       then Nothing
       else Just (i, s)

parseInteger' :: Int -> String -> (Int, String)
parseInteger' _ [] = (0, [])
parseInteger' x (y:ys)
  | isDigit(y) = parseInteger' (x * 10 + (ord y)) ys
  | otherwise  = (x, (y:ys))
