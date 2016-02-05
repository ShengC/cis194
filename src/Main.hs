module Main where

--import qualified Lecture1.Note as Note
--import qualified Lecture1.Exercise as Exercise
--import Lecture2.LogAnalysis
--import Lecture2.Log (logs)
--import Lecture3.Golf (histogram)
--import Lecture4.Exercise (foldTree, myFoldl, sieveSundaram)
--import Lecture5.Calc
--import Lecture6.Fibonacci
--import Data.Monoid
--import Lecture7.Note
--import qualified Lecture7.JoinListBufEditor
--import qualified Lecture8.Party
--import Lecture8.Employee
--import Data.Tree
--import Lecture11.AParser
--import Data.Char
--import Lecture11.SExpr
--import Lecture12.Risk
--import Control.Monad.Random
--import Lecture9.HW09
--import Lecture11.Stlc
import Lecture11.SpliceFunctions
import Lecture11.SpliceFunctionsApp

main :: IO ()
main = undefined
--  do bf <- evalRandIO $ successProb (Battlefield 7 10)
--     putStrLn . show $ bf
--main = putStrLn . show $ runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
--main = putStrLn . show $ runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
--main = Lecture8.Party.main
--  do putStrLn . show $ maxFun testCompany --maxFun (testCompany { subForest = (map (\tree -> tree { subForest = [] }) (subForest testCompany))})
--main = Lecture7.JoinListBufEditor.main
--  do putStrLn . show . getANDBool $ ANDBool True <> ANDBool False
--     putStrLn . show . getORBool $ ORBool True <> ORBool False
--  do putStrLn . show $ fibs3
--  do putStrLn . show $ x ^ 4
--     putStrLn . show $ (1 + x) ^ 5
--     putStrLn . show $ (x ^ 2 + x+ 3) * (x - 5)
--  do putStrLn . show . take 20 $ streamToList ruler
--  do putStrLn . show $ (add (lit 3) (var "x") :: VarExprT)
--  do putStrLn . show $ compile "(2+3)*5"
--  do putStrLn . show . reify $ mul (add (lit 2) (lit 3)) (lit 4)
--  do putStrLn . show $ sieveSundaram 10
--  do putStrLn . show $ myFoldl (flip (:)) [] [1..4]
--  do putStrLn . show $ foldTree "ABCDEFGHIJ"
--  do putStrLn (histogram  [1,1,1,5])
--  do putStrLn . show $ whatWentWrong . map parseMessage' $ logs
--  do putStrLn . show $ parseMessage' "E 2 562 help help"
--     putStrLn . show $ parseMessage' "I 29 la la la"
--     putStrLn . show $ parseMessage' "This is not in the right format"
--  do putStrLn . show $ Exercise.hanoi 2 "a" "b" "c"
--  do putStrLn . show $ Exercise.validate 4012888888881881
--     putStrLn . show $ Exercise.validate 4012888888881882
