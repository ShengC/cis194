module Lecture11.Note where

import Control.Applicative

(>**) :: Applicative f => f a -> f b -> f b
(>**) = liftA2 (flip const)

sequenceB :: Applicative f => [f a] -> f [a]
sequenceB = foldr (liftA2 (:)) (pure [])

mapB :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapB f = sequenceB . map f

replicateB :: Applicative f => Int -> f a -> f [a]
replicateB n = sequenceB . replicate n
