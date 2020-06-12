module SimpleExample where

import BinRelationV3 
import QuesGeneratorV3
import ExecuteQuiz
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State


import qualified Data.List as L

motherchildL = [("Alice", "Bob"  ), 
                ("Alice", "Rachel"), 
                ("Jane" , "Alice" ), 
                ("April", "Jane"  )]

fatherchildL = [("Patrick", "Bob"    ), 
                ("Patrick", "Rachel" ), 
                ("Matthew", "Patrick"), 
                ("Leonard", "Matthew")]

-- * create relation using constructors (from list)

motherchildR = fromList motherchildL

fatherchildR = fromListWithNames "father-child" "father" "child" fatherchildL


-- * Questions can be generatd manually wihtout calling any smart constructors
-- | Question String String
q1 = MCQ MA (S L "Alice" "is mother of")
        [  Item "Bob"    "Correct"           1 
          ,Item "April"  "Partially Correct" 0.5 
          ,Item "Jane"   "Incorrect"         0.25 
          ,Item "Rachel" "Correct"           1 ]
          
-- * Question created with smart constructors for statement 
--   and automated look up functions for answers and distractors

-- | statement: a L statement with 1st domain element 
-- | question string made with domain name (dname)
s2 = getDmStatement L 0 getQStringDname fatherchildR
-- | distractors
d2 = getDmDistractors s2 fatherchildR 
-- | answers
a2 = getDmAnswers s2 fatherchildR 

-- | getting rid of statement value from distractor list
{- why? - domain and range have same type, distractor can contain statement value
        as that's also an incorrect option which would be 'not wrong' but silly to add 
        as a incorrect choice 
-}
d2' = [ s | s <- d2, s /= (geta s2)] 
q2 = MCQ SA s2 (getOptsFromAnsDists 1 a2 [True]         [1.0] 
                                    2 d2' [False, False] [0.0, 0.0])

-- * Question created with smart constructors and functions
s6 = getRnStatement R 3 getQStringDname fatherchildR
d6 = getRnDistractors s6 fatherchildR 
a6 = getRnAnswers s6 fatherchildR

-- | getting rid of statement value from distractor list
d6' = [ s | s <- d6, s /= (geta s6)] 

q6 = MCQ SA s6 (getOptsFromAnsDists 1 a6 [True]         [1.0] 
                                    2 d6' [False, False] [-0.25, -0.25])
          
-- * create relation from other relations 

-- | Get `parent-child` relation from union of `father-child` and `mother-child`   

--  >>> toList parentchildR 
-- [
-- ("Alice","Bob"),("Alice","Rachel"),
-- ("April","Jane"),("Jane","Alice"),
-- ("Leonard","Matthew"),("Matthew","Patrick"),
-- ("Patrick","Bob"),("Patrick","Rachel")
-- ]
parentchildR :: Relation String String
parentchildR = setNames "parent-child" "parent" "child" 
                          (union motherchildR fatherchildR)

-- * Question created with smart constructors and functions

-- | statement: a R statement with 3rd range element 
-- | question string made with domain name (dname)
s61 = getDmStatement R 2 getQStringRname parentchildR
-- | answers and distractors
d61 = getDmDistractors s61 parentchildR 
a61 = getDmAnswers s61 parentchildR 

-- | getting rid of statement value from distractor list
d61' = [ s | s <- d61, s /= (geta s61)] 
--q6 :: Question String String Bool
q61 = MCQ SA s61 (getOptsFromAnsDists 1 a61 [True]         [1.0] 
                                      2 d61' [False, False] [0.0, 0.0])
 
-- | Get `grandparent-child` relation from 
--   from composing parentchildR with parentchildR
--   Ex: a `is parent to` b && b `is parent to` c 
--           -> a `is grandparent to` c

--  >>> toList grandParentChild 
-- [
-- ("April","Alice"),("Jane","Bob"),
-- ("Jane","Rachel"),("Leonard","Patrick"),
-- ("Matthew","Bob"),("Matthew","Rachel")
-- ]
grandParentChildR :: Relation String String
grandParentChildR = compose parentchildR parentchildR 


-- | Get `greatgrandparent-child` relation from 
--   from composing grandParentChildR with parentchildR
--   Ex: a `is grandparent to` b && b `is parent to` c 
--           -> a `is great-grandparent to` c

--  >>> toList greatgrandParentChildR 
-- [
--  ("April","Bob"),("April","Rachel"),
--  ("Leonard","Bob"),("Leonard","Rachel")
-- ]

greatgrandParentChildR :: Relation String String
greatgrandParentChildR = compose grandParentChildR parentchildR

-- | Finally combining all generated question with a title
q3 = q1 :++: q2 :++: q6 :++: q61
q3n = Quiz "Family Tree Questions: " q3

------------------------------------------------------------------------------------------
-- * Math
-- | Question String Int
q4 = MCQ SA (S L ("3-1") "is equal to")
          (getOptsFromAnsDists 
          1  [2]         [True]  [1.0]                -- ans
          3  [1, 3, 4]   [False, False, False] [0.0, 0.0, 0.0] )                -- dist

q4n = Quiz "Math Questions: " q4 
q5n = q3n :++: q4n 

----------------------------------------------------------------------------------

-- * Persian Social Hierarchy 

societiesElems = ["Persian Society"]

persianSocietyCategory = ["Upper Class", "Middle Class", "Lower Class"]

upperClassElems  = ["King", "Royal Family", "Royal Priests"]
middleClassElems = ["Aristocrats", "Military"]
lowerClassElems  = ["Traders", "Artisans"]


-- * Create relations using constructors

containsUpperR  = fromDomRanFullRWNames "has member" "class" "member" 
                                  ["Upper Class"] upperClassElems

containsMiddleR = fromDomRanFullRWNames "has member" "class" "member" 
                                  ["Middle Class"] middleClassElems

containsLowerR  = fromDomRanFullRWNames "has member" "class" "member" 
                                  ["Lower Class"] lowerClassElems

catPersianSociety = fromDomRanFullRWNames "categorized-into" "society" "class" 
                                  ["Persian Society"] persianSocietyCategory

categorySociety   = fromDomRanFullRWNames "categorized-into" "" "" 
                                  ["Society"] societiesElems

-- * Create relation with combinators

-- | Builds all `class` contains `member` relation pairs with union

contains = unions  [ containsUpperR      -- keeps First elem names/ annotaions
                   , containsMiddleR
                   , containsLowerR
                   ]
-- | Builds a higher order `contains` relation with compose 
-- Example: if a `categorized into` b
--          if b `has member` c
--        then a `has member` c  

--  >>> getNames containsO2
-- ["has member","class","member"]
containsO2 = compose catPersianSociety contains


s7 = S L (getDmValue 1 contains) "has member"
d7 = getDmDistractors s7 contains 
a7 = getDmAnswers s7 contains 

q7 = MCQ MA s7 (getOptsFromAnsDists 1 a7 [True]                [1.0] 
                                 3 d7 [False, False, False] [0.0, 0.0, 0.0])

containsEq   = setRelatedBy "equivalent" (contains `prod` contains) 
containsO2Eq = setRelatedBy "equivalent" (containsO2 `prod` containsO2) 
containsUni  = containsEq `union` containsO2Eq

-- | Question String String
s8 = getDmStatement L 1 getQStringDname containsUni
d8 = getDmDistractors s8 containsUni 
a8 = getDmAnswers s8 (removeEqualFmEquiv containsUni)

opts8 = (getOptsFromAnsDists 1 a8 [True]                [1.0] 
                             3 d8 [False, False, False] [0.0, 0.0, 0.0])

q8 = MCQ SA s8 opts8

q8n = Quiz "Social Studies Questions: " (q7 :++: q8)

-----------------------------------------------------------------------------------
-- ** Final quiz combining all questions from all domains
finalQuiz = Quiz "Mid Test: " (q5n :++: q8n)
