{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture12.Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf =
  do a <- replicateM (attackers bf - 1 `min` 3) die
     d <- replicateM (defenders bf `min` 2) die
     let s  = (reverse . sort $ a) `zip` (reverse . sort $ d)
         ma = length . filter (\(x, y) -> x <= y) $ s
         md = length . filter (\(x, y) -> x > y) $ s
     return Battlefield { attackers = attackers bf - ma, defenders = defenders bf - md }

invade :: Battlefield -> Rand StdGen Battlefield
invade bf =
  do bf' <- battle bf
     if (attackers bf' == 1 || defenders bf' == 0)
        then return bf'
        else (invade bf')

successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  do results <- replicateM 1000 (invade bf)
     return ((fromIntegral . length . filter (==0) . map defenders $ results) / 1000)
