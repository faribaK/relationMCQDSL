{-#LANGUAGE GADTs #-}

module ExecuteQuiz (

   execQuiz,

) where

import QuesGeneratorV3
import Control.Applicative



check :: Int -> [Options a v] -> IO v
check 0 ((Item _ v):is) = return v
check n ((Item _ v):is) = check (n-1) is

execQuiz :: Question a b v -> IO v
execQuiz (MCQ (L a s) optionL) = do
    putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\n"
    showOptions 1 optionL
    ans <- readLn :: IO Int
    check (ans - 1) optionL            
execQuiz (MCQ (R b s) optionL) = do
    putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\n"
    showOptions 1 optionL
    ans <- readLn :: IO Int
    check (ans - 1) optionL
execQuiz (left :++: right) = do
    la <- execQuiz left
    ra <- execQuiz right
    return (la, ra)
execQuiz (left :||: right) = do
    la <- execQuiz left
    ra <- execQuiz right
    return (la, ra)
execQuiz (Quiz n ques) = do
    putStrLn $ "\n" ++ n
    putStrLn $ take (length n) $ repeat '='
    execQuiz ques


showOptions :: (Show a) => Int -> [Options a v] -> IO Int
showOptions num []               = return $ succ num 
showOptions num ((Item a _): is) = do
            putStrLn $ "[" ++ show num ++ "] " ++ show a
            showOptions (succ num) is
