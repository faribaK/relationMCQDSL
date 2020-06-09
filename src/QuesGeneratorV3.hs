{-# LANGUAGE GADTs #-}

module QuesGeneratorV3 where

import BinRelationV3

import qualified Data.List as L
import qualified Data.Set as S

type Distractors a = [a]
type Answers a = [a]

-- | A question statement with a key K, value a and String s
-- | K indicates whether a is from domain (K = L) or range (K = R) of a Relation
-- | s holds the string to ask the question [can be relation name or a string made with domain or range name]
-- | Example :
-- | For a `father-son` relation pair (Mat, Ron), question statements can be like
-- | S L Mat "is father of" | printed as: Mat is father of ___? Or,
-- | S R Ron "is father of" | printed as: ___ is father of Ron? Or,
-- | S L Ron "is son of"    | printed as: Ron is  son   of ___?
data Statement a = S K a String deriving (Eq, Show)

-- | L or R is used when priting the statament
-- | to indicate wheter blank should be on the left or right 
data K = L | R deriving (Eq, Show)

data Options a v where
 Item  :: a -> v -> Double -> Options a v
     deriving (Eq, Show)
--  (:+:) :: Options a -> Options a -> Options a


data Question a b where
 MCQ     :: (Show a, Show b, Show v) => Statement a  -> [Options b v]  -> Question a b 
 MCQMA   :: (Show a, Show b, Show v) => Statement a  -> [Options b v]  -> Question a b 
 (:++:)  :: Question a b -> Question a b  -> Question a b 
 (:||:)  :: Question a b -> Question c d  -> Question (a, c) (b, d) 
 Quiz    :: String       -> Question a b  -> Question a b 
 
infixl 3 :++:
infixl 2 :||: 

-- * smart constructors

-- | get an question statetment from the a Relation 
getStatement :: K -> a -> String -> Statement a
getStatement k a s = S k a s

getLStatement :: Ord a => Int -> Relation a b -> Statement a 
getLStatement n r = S L (getNthDomain n r) (getQStringRelatedBy r)

getRStatement :: Ord b => Int -> Relation a b -> Statement b
getRStatement n r = S R (getNthRange n r) (getQStringRelatedBy r)

getOptsFromList :: [a] -> [v] -> [Double] -> [Options a v]
getOptsFromList (x:xs) (b:bs) (d:ds) = [Item x b d] ++ getOptsFromList xs bs ds
getOptsFromList _       _      _     = []

getOptsFromAnsDists :: Int -> Answers a -> [v] -> [Double] -> Int -> Distractors a -> [v] -> [Double] -> [Options a v]
getOptsFromAnsDists an ans v1 d1 dn dist v2 d2 =  a ++ d
                                       where  a = (getOptsFromList (take an ans)  v1 d1)
                                              d = (getOptsFromList (take dn dist) v2 d2)
-- * destructors
geta :: Statement a -> a
geta (S _ a s) = a

getS :: Statement a -> String
getS (S _ a s) = s

getvOpt :: Options a v -> v
getvOpt (Item a v d) = v

getaOpt :: Options a v -> a
getaOpt (Item a v d) = a

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
getLDistractors (S _ a s) r = [e | e <- (getRange r), e `notElem` (getRangeOf a r)]

getRDistractors :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Distractors a 
getRDistractors (S _ b s) r = [e | e <- (getDomain r), e `notElem` (getDomainOf b r)]   

getLAnswers :: (Eq a, Eq b, Ord a, Ord b) => Statement a -> Relation a b -> Answers b 
getLAnswers (S _ a s) r = getRangeOf a r

getRAnswers :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Answers a 
getRAnswers (S _ b s) r = getDomainOf b r 

getLDistractorsFrmFalse :: (Eq a, Eq b, Ord a, Ord b) => Statement a -> Relation a b -> Distractors b 
getLDistractorsFrmFalse (S _ a s) r = getRangeOfFrmFalseEl a r

getRDistractorsFrmFalse :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Distractors a 
getRDistractorsFrmFalse (S _ b s) r = getDomainOfFrmFalseEl b r  

-- * list manipulation 

getNthPermutation :: Int -> [a] -> [a]
getNthPermutation n l = (L.permutations l) !! n

--instance (Show a, Show v) => Show (Options a v) where 
--  show (Empty)  = "\tEmpty"
--    show (Item a t)  = "Item " ++ (show a) ++ " " ++ (show t)   
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