{-#LANGUAGE GADTs #-}

module ExecuteQuiz (

   execQuiz, filterInput

) where

import QuesGeneratorV3
import Data.Char
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import System.Random


prettyPrint :: (Show v) => (Int, (v, Double)) -> String
prettyPrint (i, (v, d)) = ((show i) ++ " - " ++ (show v) ++ " - " ++ (show d) ++ "\n")

printCollect :: (Show v) => [(Int, (v, Double))] -> Writer String Double
printCollect []     = tell "\n" >> return 0.0
printCollect (v:vs) = tell (prettyPrint v) >> printCollect vs >>= return . ((+) ((snd.snd) v))

check :: [Options a v] -> Int -> (v, Double)
check ((Item _ v d):is) 0 = (v,d)
check ((Item _ v _):is) n = check is (n-1) 

checkM :: [Options a v] -> [Int] -> [(Int, (v, Double))]
checkM os []     = []
checkM os (n:ns) = let vs = checkM os ns 
                       v  = check  os (n-1)
                   in ((n, v):vs)

execQuiz :: Question a b -> StateT Int IO (Writer String Double)
execQuiz (MCQ k s optionL) = do
    (randOL, ansL) <- liftIO $ getAnswers k s optionL             -- ([Options b v], [Int]) 
    n <- get
    put (n+1)
    let qh = "Q" ++ (show n) ++ ":\n"                             -- String
        w1 = tell $ qh ++ (take (length qh) $ repeat '-') ++ "\n" -- Writer String ()
        vs = (checkM randOL ansL)                                 -- [(Int, (v, Double))]
        w2 = (printCollect vs)                                    -- Writer String Double 
    return (w1 >> w2) 
execQuiz (left :++: right) = do
    la <- execQuiz left 
    ra <- execQuiz right
    return (liftM2 (+) la ra)
execQuiz (Quiz name ques) = do
    liftIO $ putStrLn $ "\n" ++ name
    liftIO $ putStrLn $ take (length name) $ repeat '='
    execQuiz ques 

getAnswers :: (Show a, Show b, Show v) => NumA -> Statement a  -> [Options b v] -> IO ([Options b v], [Int]) 
getAnswers na st optionL = do
    printStatement na st
    randOL <- shuffle optionL
    showOptions 1 randOL
    ans <- getLine 
    let list = (filterInput (length optionL) ans :: [Int])
        list' = if na == SA then take 1 list else list
    return (randOL, list')
      

printStatement :: (Show a) => NumA -> Statement a -> IO ()
printStatement SA (S L a s) = 
    putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose one:\n"
printStatement SA (S R b s) = 
    putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose one:\n"
printStatement MA (S L a s) = 
    putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose all that apply:\n"
printStatement MA (S R b s) = 
    putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose all that apply:\n"
           


showOptions :: (Show a) => Int -> [Options a v] -> IO Int
showOptions num []                 = return $ succ num 
showOptions num ((Item a _ _): is) = do
            putStrLn $ "[" ++ show num ++ "] " ++ show a
            showOptions (succ num) is

filterInput :: Int -> String -> [Int]
filterInput i = filter (<=i) . (map read . (words . (filter (\x -> isSpace x ||  isDigit x)))) 

-- | shuffle a list
-- | (https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell)
shuffle :: [a] -> IO [a]
shuffle l | length l < 2 = return l 
          | otherwise    = do
                 i <- randomRIO (0, length(l)-1)
                 r <- shuffle (take i l ++ drop (i+1) l)
                 return (l!!i : r)

-- 
-- isValid :: Int -> [Int] -> Bool
-- isValid max s = length s >= 1
--             && all isNumber s
--             

-- execQuiz (MCQ SA (S L a s) optionL) = do
--     putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose one:\n"
--     showOptions 1 optionL
--     ans <- readLn :: IO Int
--     vs <- checkM [ans] optionL     
--     return (tell ("Q" ++ (show 0) ++ ": ") >> printCollect vs)
-- execQuiz (MCQ SA (S R b s) optionL) = do
--     putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose one:\n"
--     showOptions 1 optionL
--     ans <- readLn :: IO Int
--     vs <- checkM [ans] optionL
--     return (tell ("Q" ++ (show 0) ++ ": ") >> printCollect vs)
-- execQuiz (MCQ MA (S L a s) optionL) = do
--     putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose all correct ones (as a list):\n"
--     showOptions 1 optionL
--     ans <- getLine 
--     let list = (read ans :: [Int])
--     vs <- checkM list optionL
--     return (tell ("Q" ++ (show 0) ++ ": ") >> printCollect vs)            
-- execQuiz (MCQ MA (S R b s) optionL) = do
--     putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose all correct ones (as a list):\n"
--     showOptions 1 optionL
--     ans <- getLine 
--     let list = (read ans :: [Int])
--     vs <- checkM list optionL
--     return (tell ("Q" ++ (show 0) ++ ": ") >> printCollect vs) 

-- getAnswers SA (S L a s) optionL = do
--     printStatement SA L a s
--     --putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose one:\n"
--     showOptions 1 optionL
--     ans <- getLine 
--     let list = take 1(filterInput (length optionL) ans :: [Int])
--     return (optionL, list)
-- getAnswers SA (S R b s) optionL = do
--     printStatement SA R b s
--     -- putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose one:\n"
--     showOptions 1 optionL
--     ans <- getLine 
--     let list = take 1 (filterInput (length optionL) ans :: [Int])
--     return (optionL, list)
-- getAnswers MA (S L a s) optionL = do
--     putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose all that apply:\n"
--     showOptions 1 optionL
--     ans <- getLine 
--     let list = (filterInput (length optionL) ans :: [Int])
--     return (optionL, list)      
-- getAnswers MA (S R b s) optionL = do
--     putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose all that apply:\n"
--     showOptions 1 optionL
--     ans <- getLine 
--     let list = (filterInput (length optionL) ans :: [Int])
--     return (optionL, list)      