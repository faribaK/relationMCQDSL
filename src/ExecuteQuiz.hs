{-#LANGUAGE GADTs #-}

module ExecuteQuiz (

   execQuiz, 

) where

import QuesGeneratorV3
import Control.Applicative
import Text.Parsec hiding(digit)
import Text.Parsec.Token
import Control.Monad
import Control.Monad.Trans.Writer.Strict
--import System.Random (newStdGen)
--import System.Random.Shuffle (shuffle')


printCollect ::  (Show v) => [(v, Double)] -> Writer String Double
printCollect [] = tell "\n" >> return 0.0
printCollect ((v, d):ls) = do
    tell ((show v) ++ " --Score:" ++ (show d) ++ "\n")
    ds <- printCollect ls
    return (d + ds)


check :: Int -> [Options a v] -> IO (v, Double)
check 0 ((Item _ v d):is) = return (v,d)
check n ((Item _ v _):is) = check (n-1) is

checkMA :: [Int] -> [Options a v] -> IO [(v, Double)]
checkMA [] is     = return []
checkMA (n:ns) is = do vs <- checkMA ns is 
                       v  <- check (n-1) is 
                       return (v:vs)

execQuiz :: Question a b -> IO (Writer String Double)
execQuiz (MCQ (S L a s) optionL) = do
    putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose one:\n"
    showOptions 1 optionL
    ans <- readLn :: IO Int
    v <- check (ans-1) optionL     
    return (printCollect [v])
execQuiz (MCQ (S R b s) optionL) = do
    putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose one:\n"
    showOptions 1 optionL
    ans <- readLn :: IO Int
    v <- check (ans-1) optionL
    return (printCollect [v])
execQuiz (MCQMA (S L a s) optionL) = do
    putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose all correct ones (as a list):\n"
    showOptions 1 optionL
    ans <- getLine 
    let list = (read ans :: [Int])
    vs <- checkMA list optionL
    return (printCollect vs)            
execQuiz (MCQMA (S R b s) optionL) = do
    putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose all correct ones (as a list):\n"
    showOptions 1 optionL
    ans <- getLine 
    let list = (read ans :: [Int])
    vs <- checkMA list optionL
    return (printCollect vs)  
execQuiz (left :++: right) = do
    la <- execQuiz left
    ra <- execQuiz right
    return (liftM2 (+) la ra) 
execQuiz (left :||: right) = do
    la <- execQuiz left
    ra <- execQuiz right
    return (liftM2 (+) la ra) 
execQuiz (Quiz n ques) = do
    putStrLn $ "\n" ++ n
    putStrLn $ take (length n) $ repeat '='
    execQuiz ques


showOptions :: (Show a) => Int -> [Options a v] -> IO Int
showOptions num []                 = return $ succ num 
showOptions num ((Item a _ _): is) = do
            putStrLn $ "[" ++ show num ++ "] " ++ show a
            showOptions (succ num) is

