{-# LANGUAGE GADTs #-}

module QuesGeneratorV3 where

import BinRelationV3

import qualified Data.List as L
import qualified Data.Set as S

type Distractors a = [a]
type Answers a = [a]


-- | A question statement with a key K, value a and String s
-- | String holds the string to ask the question [can be relation name or a string made with domain or range name]
-- | a holds a value from domain or range of a Relation and has respective type
-- | K indicates whether blank should be placed on the left (K = L) or right (K = R) of a Relation
-- | Example :
-- | For a `father-son` relation pair (Mat, Ron), question statements can be like
-- | S L Mat "is father of" | printed as: Mat is father of ___? Or,
-- | S R Ron "is father of" | printed as: ___ is father of Ron? Or,
-- | S L Ron "is son of"    | printed as: Ron is  son   of ___?
data Statement a = S K a String deriving (Eq, Show)

-- | L or R is used when priting the statament
-- | to indicate wheter blank should be on the left or right 
data K = L | R deriving (Eq, Show)

-- | Options for multiple choice questions 
-- | a holds a answer or distractor value
-- | v holds an explanation or description for the option 
-- | Double holds the score for the option
data Options a v where
 Item  :: a -> v -> Double -> Options a v
     deriving (Eq, Show)

-- | choose one (SA) or all (MA) that apply
data NumA = SA | MA deriving (Eq, Show)

data Question a b where
 MCQ     :: (Show a, Show b, Show v) => NumA -> Statement a  -> [Options b v]  -> Question a b 
 (:++:)  :: Question a b -> Question c d  -> Question (a, c) (b, d) 
 Quiz    :: String       -> Question a b  -> Question a b 
infixl 2 :++:

-- * smart constructors

-- | get an question statetment from the a Relation 
getStatement :: K -> a -> String -> Statement a
getStatement k a s = S k a s

-- | get a K statement with nth domain element
getDmStatement :: Ord a => K -> Int -> (Relation a b -> String) -> Relation a b -> Statement a 
getDmStatement k n f r = S k (getNthDomain n r) (f r)

-- | get a K statement with nth range element
getRnStatement :: Ord b => K -> Int -> (Relation a b -> String) -> Relation a b -> Statement b 
getRnStatement k n f r = S k (getNthRange n r) (f r)


getOptsFromList :: [a] -> [v] -> [Double] -> [Options a v]
getOptsFromList (x:xs) (b:bs) (d:ds) = [Item x b d] ++ getOptsFromList xs bs ds
getOptsFromList _       _      _     = []

-- | generate option list from answer and distractor list
-- | numofanswers -> answers -> explanations -> scores -> numoddistractors -> distractors -> explanations -> scores
getOptsFromAnsDists :: Int -> Answers a -> [v] -> [Double] -> Int -> Distractors a -> [v] -> [Double] -> [Options a v]
getOptsFromAnsDists an ans v1 d1 dn dist v2 d2 =  a ++ d
                                       where  a = (getOptsFromList (take an ans)  v1 d1)
                                              d = (getOptsFromList (take dn dist) v2 d2)
-- * destructors
geta :: Statement a -> a
geta (S _ a s) = a

getS :: Statement a -> String
getS (S _ a s) = s

setS :: String -> Statement a -> Statement a
setS s (S k a _) = S k a s

getvOpt :: Options a v -> v
getvOpt (Item a v d) = v

getaOpt :: Options a v -> a
getaOpt (Item a v d) = a

-- * utility Function to get question element from a a Relation

-- | get n-th domain value from Relation
getDmValue :: Ord a => Int -> Relation a b -> a 
getDmValue n r = (getNthDomain n r)

-- | get n-th range value from Relation
getRnValue :: Ord b => Int -> Relation a b -> b 
getRnValue n r = (getNthRange n r)

-- | create question string/ phrase with relatedBy of Relation
getQStringRelatedBy :: Relation a b -> String
getQStringRelatedBy r = ("has a " ++ getRelatedBy r ++ " relationship with")

-- | create question string/ phrase with domain name 
getQStringDname :: Relation a b -> String
getQStringDname r = ("is " ++ getDomainName r ++ " of")

-- | create question string/ phrase with range name 
getQStringRname :: Relation a b -> String
getQStringRname r = ("is " ++ getRangeName r ++ " of")

-- | get incorrect choices for a domain value
getDmDistractors :: (Eq a, Eq b, Ord a, Ord b) => Statement a -> Relation a b -> Distractors b 
getDmDistractors (S _ a s) r = [e | e <- (getRange r), e `notElem` (getRangeOf a r)]

-- | get incorrect choices for a range value
getRnDistractors :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Distractors a 
getRnDistractors (S _ b s) r = [e | e <- (getDomain r), e `notElem` (getDomainOf b r)]   

-- | get all correct choices for a domain value
getDmAnswers :: (Eq a, Eq b, Ord a, Ord b) => Statement a -> Relation a b -> Answers b 
getDmAnswers (S _ a s) r = getRangeOf a r

-- | get correct choices for a range value
getRnAnswers :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Answers a 
getRnAnswers (S _ b s) r = getDomainOf b r 

-- | get incorrect choices from false elements (utility has not been fully investigated)

getDmDistractorsFrmFalse :: (Eq a, Eq b, Ord a, Ord b) => Statement a -> Relation a b -> Distractors b 
getDmDistractorsFrmFalse (S _ a s) r = getRangeOfFrmFalseEl a r

getRnDistractorsFrmFalse :: (Eq a, Eq b, Ord a, Ord b) => Statement b -> Relation a b -> Distractors a 
getRnDistractorsFrmFalse (S _ b s) r = getDomainOfFrmFalseEl b r  

