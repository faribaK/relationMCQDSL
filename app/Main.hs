module Main where

import BinRelationV3 
import QuesGeneratorV3
import ExecuteQuiz
import SimpleExample
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State

main :: IO ()
main = runStateT (execQuiz finalQuiz) 1 >>= 
       \(w, _) -> return (runWriter w)  >>= 
       \(a, s) -> putStr (s ++ "\n Total Score: " ++ (show a)++"\n") 
       
-- main = (execQuiz finalQuiz) >>= (putStrLn . execWriter)
-- main = (execQuiz finalQuiz) >>= \s -> putStrLn (show s)
-- main = (execQuiz finalQuiz) >>= \d -> print d >> (calcScore finalQuiz d)
