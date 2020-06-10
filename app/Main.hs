module Main where

import BinRelationV3 
import QuesGeneratorV3
import ExecuteQuiz
import SimpleExample

main :: IO ()
main = (execQuiz finalQuiz) >>= return . runWriter >>= \(a, s) -> putStr (s ++ "\n Total Score: " ++ (show a) "\n") 

-- main = (execQuiz finalQuiz) >>= (putStrLn . execWriter)
-- main = (execQuiz finalQuiz) >>= \s -> putStrLn (show s)
-- main = (execQuiz finalQuiz) >>= \d -> print d >> (calcScore finalQuiz d)
