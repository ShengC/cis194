-- CIS 194 Homework 2

module Lecture2.Log where

import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file

logs :: [String]
logs =
  [ "I 6 Completed armadillo processing"
  , "I 1 Nothing to report"
  , "E 99 10 Flange failed!"
  , "I 4 Everything normal"
  , "I 11 Initiating self-destruct sequence"
  , "E 70 3 Way too many pickles"
  , "E 65 8 Bad pickle-flange interaction detected"
  , "W 5 Flange is due for a check-up"
  , "I 7 Out for lunch, back in two time steps"
  , "E 20 2 Too many pickles"
  , "I 9 Back from lunch"
  ]
