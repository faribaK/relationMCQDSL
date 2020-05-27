module SimpleExample where

import BinRelationV3 
import QuesGeneratorV3
import ExecuteQuiz

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

fatherchildR = fromListWithNames "father-child" "" "" fatherchildL

-- ["Bob", "Rachel"]
-- ["April", "Jane"]
q1:: Question String String Bool
q1 = MCQ (L "Alice" "is mother of")
        [  Item "Bob" True 
          ,Item "April" False 
          ,Item "Jane"  False 
          ,Item "Rachel" True ]
          

s2 = getLStatement 1 fatherchildR
d2 = getLDistractors s2 fatherchildR 
a2 = getLAnswers s2 fatherchildR 

q2 :: Question String String Bool
q2 = MCQ s2 (getOptsFromAnsDists 1 a2 2 d2)

s6 = getRStatement 1 fatherchildR
d6 = getRDistractors s6 fatherchildR 
a6 = getRAnswers s6 fatherchildR 

q6 :: Question String String Bool
q6 = MCQ s6 (getOptsFromAnsDists 1 a6 2 d6)
          
q3 = q1 :++: q2 :++: q6
q3n = Quiz "Family Tree Questions: " q3

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

------------------------------------------------------------------------------------------

q4:: Question String Int Bool
q4 = MCQ (L ("3-1") "is equal to")
          (getOptsFromAnsDists 
          1  [2]                         -- ans
          2  [1, 3, 4] )                 -- dist

q4n = Quiz "Math Questions: " q4 
q5n = q3n :||: q4n 

----------------------------------------------------------------------------------
-- universities
-- Persian Social Hierarchy 

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

catPersianSociety = fromDomRanFullRWNames "categorized-into" "society" "category" 
                                  ["Persian Society"] persianSocietyCategory

categorySociety   = fromDomRanFullRWNames "categorized-into" "" "" 
                                  ["Society"] societiesElems

-- * Create relation with combinators

-- | Builds all `class` contains `member` relation pairs with union

contains = unions  [containsUpperR      -- keeps First elem names/ annotaions
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


s7 = L (getLValue 1 contains) "has member"
d7 = getLDistractors s7 contains 
a7 = getLAnswers s7 contains 

q7 :: Question String String Bool
q7 = MCQ s7 (getOptsFromAnsDists 1 a7 3 d7)

q7n = Quiz "Social Studies Questions: " q7

finalQuiz = q5n :||: q7n











-- is above

-- generalOfficers = ["General", "Lieutenant General", "Major General"]
-- fieldOfficers = ["Colonel", "Lieutenant Colonel", "Major Colonel"]
-- 
-- CommissionedofficersHierarchyL = []
-- 
-- generalOfficersHierarchyL = [("General", "Lieutenant General")
--                             ,("Lieutenant General", "Major General")
--                             ]
-- 
-- fieldOfficersHierarchyL = [("Colonel", "Lieutenant Colonel")
--                           ,("Lieutenant Colonel", "Major Colonel")       
--                           ]
--    
-- -- | create relation using constructors (from list)
-- 
-- generalOfficersHierarchyR = fromList "is-ranked-above" "" "" generalOfficersHierarchyL
-- 
-- fieldOfficersHierarchyR = fromListWithNames "is-ranked-above" "" "" fatherchildL
--    
   
   