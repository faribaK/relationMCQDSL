{-# LANGUAGE GADTs #-}

module QuesGeneratorV3 where

import BinRelationV3

import qualified Data.Set as S

type Distractors a = [a]
type Answers a = [a]

data Statement a = L a String | R a String deriving (Show)

data Options a v where
 Item  :: a -> v -> Options a v
--  (:+:) :: Options a -> Options a -> Options a
 
data Question a b v where
 MCQ     :: (Show a, Show b, Show v) => Statement a  -> [Options b v]  -> Question a b v
-- RandMCQ :: Statement a  -> IO [Options b] -> Question a b
 (:++:)  :: Question a b v1 -> Question a b v2 -> Question a b (v1, v2)
 (:||:)  :: Question a b v1 -> Question c d v2
                                           -> Question (a, c) (b, d) (v1, v2)
 Quiz    :: String       -> Question a b v -> Question a b v
 
infixl 3 :++:
infixl 2 :||: 

-- * smart constructors

getLStatement :: Ord a => Int -> Relation a b -> Statement a 
getLStatement n r = L  (getNthDomain n r) (getQStringRelatedBy r)

getRStatement :: Ord b => Int -> Relation a b -> Statement b
getRStatement n r = R (getNthRange n r) (getQStringRelatedBy r)

getOptsFromList :: [a] -> Bool -> [Options a Bool]
getOptsFromList []     b  = []
getOptsFromList (x:xs) b  = [Item x b] ++ getOptsFromList xs b

getOptsFromAnsDists :: Int -> Answers a -> Int -> Distractors a -> [Options a Bool]
getOptsFromAnsDists an ans dn dist =  a ++ d
                                       where  a = (getOptsFromList (take an ans) True)
                                              d = (getOptsFromList (take dn dist) False)

-- getRandOptsFromAnsDists :: Int -> Answers a -> Int -> Distractors a -> IO [Options a Bool]
-- getRandOptsFromAnsDists an ans dn dist = shuffle (a ++ d)
--                                              where  a = (getOptsFromList (take an ans) True)
--                                                     d = (getOptsFromList (take dn dist) False)

-- * utility Function to get question element from a a Relation
getLValue :: Ord a => Int -> Relation a b -> a 
getLValue n r = (getNthDomain n r)

getRValue :: Ord b => Int -> Relation a b -> b 
getRValue n r = (getNthRange n r)

getQStringRelatedBy :: Relation a b -> String
getQStringRelatedBy r = ("has a `" ++ getRelatedBy r ++ "` relationship with")

getLDistractors :: (Eq a, Eq b, Ord a, Ord b) => Statement a -> Relation a b -> Distractors b 
getLDistractors (L a s) r = [e | e <- (getRange r), e `notElem` (getRangeOf a r)]
getLDistractors (R a s) r = []

getRDistractors :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Distractors a 
getRDistractors (R b s) r = [e | e <- (getDomain r), e `notElem` (getDomainOf b r)]   
getRDistractors (L b s) r = []

getLAnswers :: (Eq a, Eq b, Ord a, Ord b) => Statement a -> Relation a b -> Answers b 
getLAnswers (L a s) r = getRangeOf a r
getLAnswers (R a s) r = []

getRAnswers :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Answers a 
getRAnswers (R b s) r = getDomainOf b r 
getRAnswers (L b s) r = []

-- *dist

instance (Show a, Show v) => Show (Options a v) where 
--  show (Empty)  = "\tEmpty"
    show (Item a t)  = "Item " ++ (show a) ++ " " ++ (show t)   
--  show (i1 :+: i2) = show i1 ++ "\n" ++ show i2
   
-- instance (Show (Options a)) => Show [Options a] where 
--  show (x:xs)  = "\tItem " ++ (show x) ++ "\n" ++ (show xs)  

-- instance (Show a, Show b, Show v) => Show (Question a b v) where   
--    show (MCQ (L a s) opts)  = (show a) ++ " " ++ s ++ " ______" ++ "\n"
--                                  ++ map (\c -> if c==',' then '\n' else c) (show opts) ++ "\n"  
--    show (MCQ (R b s) opts)  = "______ " ++ s ++ " " ++ (show b) ++ "\n"
--                                  ++ map (\c -> if c==',' then '\n' else c) (show opts) ++ "\n"
-- --    show (RandMCQ (L a s) opts)  = (show a) ++ " " ++ s ++ " ______" ++ "\n"
-- --                                  ++ (show opts) ++ "\n"  
-- --    show (RandMCQ (R b s) opts)  = "______ " ++ s ++ " " ++ (show b) ++ "\n"
-- --                                  ++ (show opts) ++ "\n"
--    show (q1 :++: q2)             = show q1 ++ "\n" ++ show q2
--    show (q1 :||: q2)             = show q1 ++ "\n" ++ show q2
--    show (Quiz n ques)            = n ++ "\nn" ++ show ques 
   
-- * random utlity
-- shuffle :: [a] -> IO [a]
-- shuffle l = if length l < 2 
--                  then return l else do
--                           i <- randomRIO (0, length(l)-1)
--                           r <- shuffle (take i l ++ drop (i+1) l)
--                           return (l!!i : r)