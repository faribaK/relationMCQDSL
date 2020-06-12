{-#LANGUAGE GADTs #-}

module ExecuteQuiz (

   execQuiz, filterInput

) where

import QuesGeneratorV3
import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import System.Random


-- | logs exaplantion and accumulate score
printCollect :: (Show v) => [(Int, (v, Double))] -> Writer String Double
printCollect []     = tell "\n" >> return 0.0
printCollect (v:vs) = tell (prettyPrint v) >> printCollect vs >>= return . ((+) ((snd.snd) v))

prettyPrint :: (Show v) => (Int, (v, Double)) -> String
prettyPrint (i, (v, d)) = ((show i) ++ " - " ++ (show v) ++ " - " ++ (show d) ++ "\n")

-- | Note: check [] _ case has not been included as 
-- | it can never happen if used with filtered inputs (see getAnswers and filterInput)
check :: [Options a v] -> Int -> (v, Double)
check ((Item _ v d):is) 0 = (v,d)
check ((Item _ v _):is) n = check is (n-1) 

-- | check responses against options
checkM :: [Options a v] -> [Int] -> [(Int, (v, Double))]
checkM os []     = []
checkM os (n:ns) = let vs = checkM os ns 
                       v  = check  os (n-1)
                   in ((n, v):vs)

-- * Runs a quiz created in DSL
-- | provides an interactive command-line interface 
-- | displays numbered questions 
-- | randomly shuffles option list 
-- | logs explanation for selected options 
-- | accumulates score
-- | displays result (final score and explanation)
execQuiz :: Question a b -> StateT Int IO (Writer String Double)
execQuiz (MCQ k s optionL) = do
    n <- get                                                      -- get current question number
    put (n+1)                                                     -- update question number for next
    liftIO $ putStr ( "Q" ++ (show n) ++ ":\n")                   -- displays question number
    (randOL, ansL) <- liftIO $ getAnswers k s optionL             -- displays randomized options and takes response -> ([Options b v], [Int]) 
    let qh = "Q" ++ (show n) ++ ":\n"                             -- String
        w1 = tell $ qh ++ (take (length qh) $ repeat '-') ++ "\n" -- logs question number before logging feedback -> Writer String ()
        vs = (checkM randOL ansL)                                 -- checks response against randomized options -> [(Int, (v, Double))]
        w2 = (printCollect vs)                                    -- logs feedback for selected options and accumulate score -> Writer String Double 
    return (w1 >> w2)                                             
execQuiz (left :++: right) = do
    la <- execQuiz left 
    ra <- execQuiz right
    return (liftM2 (+) la ra)
execQuiz (Quiz name ques) = do
    liftIO $ putStrLn $ "\n" ++ name
    liftIO $ putStrLn $ take (length name) $ repeat '='
    execQuiz ques 

-- | display questions with shuffled options
-- | collect and filter responses
getAnswers :: (Show a, Show b, Show v) => NumA -> Statement a  -> [Options b v] -> IO ([Options b v], [Int]) 
getAnswers na st optionL = do
    printStatement na st
    randOL <- shuffle optionL
    showOptions 1 randOL
    ans <- getLine 
    let list = (filterInput (length optionL) ans :: [Int])
        list' = if na == SA then take 1 list else list
    return (randOL, list')
      
-- | prints question statement
printStatement :: (Show a) => NumA -> Statement a -> IO ()
printStatement SA (S L a s) = 
    putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose one:\n"
printStatement SA (S R b s) = 
    putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose one:\n"
printStatement MA (S L a s) = 
    putStr $ (show a) ++ " " ++ s ++ " ______." ++ "\nChoose all that apply:\n"
printStatement MA (S R b s) = 
    putStr $ "______ " ++ s ++ " " ++ (show b) ++ ".\nChoose all that apply:\n"
           
-- | prints options with numbers
showOptions :: (Show a) => Int -> [Options a v] -> IO Int
showOptions num []                 = return $ succ num 
showOptions num ((Item a _ _): is) = do
            putStrLn $ "[" ++ show num ++ "] " ++ show a
            showOptions (succ num) is

-- | discards non-numbers and values outside [1..i]
filterInput :: Int -> String -> [Int]
filterInput i = filter (liftA2 (&&) (>0) (<=i)) . (map read . (words . (filter (liftA2 (||) isSpace isDigit)))) 

--filterInput i = filter (liftA2 (&&) (>0) (<=i)) . (map read . (words . (filter (\x -> isSpace x ||  isDigit x)))) 

-- | shuffle a list, 
-- | used here for displaying randomly shuffled options each time a quiz is run
-- | (https://www.programming-idioms.org/idiom/10/shuffle-a-list/826/haskell)
shuffle :: [a] -> IO [a]
shuffle l | length l < 2 = return l 
          | otherwise    = do
                 i <- randomRIO (0, length(l)-1)
                 r <- shuffle (take i l ++ drop (i+1) l)
                 return (l!!i : r)

