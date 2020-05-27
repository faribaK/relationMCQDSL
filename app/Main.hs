module Main where

import BinRelationV3 
import QuesGeneratorV3
import ExecuteQuiz
import SimpleExample

main :: IO ()
main = (execQuiz finalQuiz) >>= \s -> putStrLn (show s)
