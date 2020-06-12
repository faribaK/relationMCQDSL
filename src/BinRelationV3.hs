module BinRelationV3 where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq 

--
-- * Relation 
--

-- * Binary relation R on A B  - subset of A x B
-- | Remarks: names once set can be used to create bulk of questions from a relation
-- |          without having to set question statement for each question [see QuesGenerator]
data Relation a b  = Relation
  { elements      :: S.Set (a, b)  -- all pair elements belong to the relation
  , falseElements :: S.Set (a, b)  -- some pair elements that do NOT belong to the relation 
  , relatedBy     :: String        -- relationship  name
  , dname         :: String        -- domain entity name 
  , rname         :: String        -- range entitiy name
  } deriving (Show, Eq, Ord)
  

-- * Constructors 

-- | builds an empty Relation 
empty :: Relation a b
empty = Relation S.empty S.empty "" "" ""

-- | builds a Relation from a single pair 
singleton :: a -> b -> Relation a b
singleton x y  = empty
  { elements      = S.singleton (x,y) 
  , falseElements = S.empty
  }

-- | builds a Relation from a List of pairs.
fromList :: (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList xs = empty
   { elements      = S.fromList xs
   , falseElements   = S.empty
   }
   
-- | bulids a `Full` relation given Domain and Range 
--   Full Relation = Domain `cartesianProduct` Range
fromDomRanFullR :: (Ord a, Ord b) => [a] -> [b] -> Relation a b
fromDomRanFullR ds rs = Relation (xs) (S.empty) "" "" ""
             where xs = S.cartesianProduct (S.fromList ds) (S.fromList rs)


-- * Combinators

-- | builds a Relation from the union of two relations: `r` and `s` 
--   union r s = (r ++ s)
--   keep names same as `r`
union :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
union r s = copyNamesFromR r (Relation
   (S.union (elements r) (elements s))
   (S.union (falseElements  r) (falseElements  s)) "" "" "")

unions :: (Ord a, Ord b) => [Relation a b] -> Relation a b
unions []     = empty
unions (x:xs) = copyNamesFromR x $ foldl union empty (x:xs)
  
-- | compose two relations: (left to right) 
--   compose r s = [(a, c) | (a, b) <- r, (b, c) <- s]
--   compose new names from r and s : (r.relatedBy r.dname s.rname)
compose :: (Ord a, Ord b, Ord c) => Relation a b -> Relation b c -> Relation a c
compose r s = Relation 
      (listcompose (S.toList (elements r)   ) (S.toList (elements s)   ) )
      (listcompose (S.toList (falseElements r)) (S.toList (falseElements s)) )
       s1 s2 s3
    where listcompose s1 s2 = S.fromList [(a, c) | (a, b1) <- s1, (b2, c) <- s2, b1 == b2]
          [s1, s2, s3] = getComposedNames r s

-- | product two relations: (left to right) 
--   product r s = r `cartesianProduct` s
--               = [((a,b) , (c,d)) | (a,b) <- r , (c,d) <- s]
--   product names of r and s
prod :: (Ord a, Ord b, Ord c) => Relation a b -> Relation c d -> Relation (a, b) (c, d)
prod r s = Relation 
      (S.cartesianProduct (elements r)      (elements s))
      (S.cartesianProduct (falseElements r) (falseElements s))
      s1 s2 s3
    where [s1, s2, s3] = getProductNames r s


-- | builds a Relation from the intersection of two relations: r and s 
--   keep names same as `r`
intersection :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
intersection r s = Relation
   (S.intersection (elements r) (elements s))
   (S.intersection (falseElements  r) (falseElements  s)) 
   s1 s2 s3
  where [s1, s2, s3] = getIntersectNames r s
   
-- | builds a Relation from the difference of two relations: r and s 
--   keep names same as `r`
difference :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
difference r s = Relation
   (S.difference (elements r) (elements s))
   (S.difference (falseElements  r) (falseElements  s)) 
   s1 s2 s3
  where [s1, s2, s3] = getDifferenceNames r s

-- | insert a relation x and y in the relation r
insert :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insert x y r = r {elements = S.insert (x, y) (elements r)}
 
-- |  delete a relation element (a, b) from the relation.
delete :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
delete x y r = r {elements = S.delete (x, y) (elements r)}

-- | remove equal from equivalence
removeEqualFmEquiv :: (Eq a) => Relation a a -> Relation a a
removeEqualFmEquiv r = r {elements = S.filter (\p -> fst p /= snd p) (elements r)}

-- * Functions 

sizeEl :: Relation a b -> Int
sizeEl r = S.size (elements r)

sizeFlaseEl :: Relation a b -> Int
sizeFlaseEl r = S.size (falseElements r)

getnElem:: Int -> Relation a b -> (a, b)
getnElem n r = S.elemAt n setElem
                     where setElem = elements r 

-- || Set false elements

insertFalseElem :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insertFalseElem x y r = r {falseElements = S.insert (x, y) (falseElements r)}

deleteFalseElem :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
deleteFalseElem x y r = r {falseElements = S.delete (x, y) (falseElements r)}

setFalseElems :: (Ord a, Ord b) => [(a, b)] -> Relation a b -> Relation a b
setFalseElems nxs r = r { falseElements = S.fromList nxs }

-- || Access and compose names

getRelatedBy :: Relation a b -> String
getRelatedBy r = relatedBy r

setRelatedBy :: String -> Relation a b -> Relation a b 
setRelatedBy s r = r{relatedBy = s}

getDomainName :: Relation a b -> String
getDomainName r = dname r

setDomainName :: String -> Relation a b -> Relation a b 
setDomainName s r = r{dname = s}

getRangeName :: Relation a b -> String
getRangeName r = rname r

setRangeName :: String -> Relation a b -> Relation a b 
setRangeName s r = r{rname = s}

getNames :: Relation a b -> [String]
getNames r = [relatedBy r, dname r, rname r]

setNames :: String -> String -> String -> Relation a b -> Relation a b 
setNames s1 s2 s3 r = setRelatedBy s1 $ setDomainName s2 $ setRangeName s3 r

-- | copy names from first relation to the second
copyNamesFromR :: Relation c d -> Relation a b -> Relation a b 
copyNamesFromR r s = setNames (relatedBy r) (dname r) (rname r)  s

-- | generate names for two `union` -ed relations
getUnionNames :: Relation a b -> Relation c d -> [String]
getUnionNames r s = [ getRelatedBy r ++ " -or- " ++ getRelatedBy s 
                    , getDomainName r
                    , getRangeName  r]
-- | generate names for two `intersection` -ed relations
getIntersectNames :: Relation a b -> Relation c d -> [String]
getIntersectNames r s = [ getRelatedBy r ++ " -and- " ++ getRelatedBy s 
                        , getDomainName r
                        , getRangeName  r]
-- | generate names for two `intersection` -ed relations
getDifferenceNames :: Relation a b -> Relation c d -> [String]
getDifferenceNames r s = [ getRelatedBy r ++ " -not- " ++ getRelatedBy s 
                         , getDomainName r
                         , getRangeName  r]
-- | generate names for two `Compose` -d relations
getComposedNames :: Relation a b -> Relation b c -> [String]
getComposedNames r s = [ getRelatedBy r ++ " -to- " ++ getRelatedBy s 
                       , getDomainName r
                       , getRangeName  s]

-- | generate names for two `Product` -ed relations
getProductNames :: Relation a b -> Relation c d -> [String]
getProductNames r s = [ "(" ++ getRelatedBy r ++ " )--( " ++ getRelatedBy s ++ ")"
                      , "(" ++ getDomainName r ++ "," ++ getDomainName s ++ ")"
                      , "(" ++ getRangeName r  ++ "," ++ getRangeName s  ++ ")"]
 
 
-- || Get domain and range 

getDomain :: Ord a => Relation a b -> [a]
getDomain r = S.toList (S.map (fst) (elements r))

getRange :: Ord b => Relation a b -> [b]
getRange r = S.toList (S.map (snd) (elements r))

getDomainSize :: Ord a => Relation a b -> Int
getDomainSize r = L.length (getDomain r) 

getRangeSize :: Ord b => Relation a b -> Int
getRangeSize r = L.length (getRange r) 

-- | get range of a particular domain element
getRangeOf :: (Eq a, Ord b) => a -> Relation a b -> [b]
getRangeOf a r = S.toList $ S.map (snd) (S.filter (\s -> fst s == a) (elements r))

-- | get domain of a particular range element
getDomainOf :: (Eq b, Ord a) => b -> Relation a b -> [a]
getDomainOf b r = S.toList $ S.map (fst) (S.filter (\s -> snd s == b) (elements r))

-- | get nth domain/range element (Unsafe) 
getNthDomain :: Ord a => Int -> Relation a b -> a
getNthDomain n r = ((getDomain r) !! n)

getNthRange :: Ord b => Int -> Relation a b -> b
getNthRange n r = ((getRange r) !! n)

-- | get nth domain/range element (Safe) 
getNthDomainSafe :: Ord a => Int -> Relation a b -> Maybe a
getNthDomainSafe n r = case (getDomain r) of
                   [] -> Nothing
                   _  -> Just ((getDomain r) !! n)

getNthRangeSafe :: Ord b => Int -> Relation a b -> Maybe b
getNthRangeSafe n r = case (getRange r) of
                   [] -> Nothing
                   _  -> Just ((getRange r) !! n)

getRangeOfFrmFalseEl :: (Eq a, Ord b) => a -> Relation a b -> [b]
getRangeOfFrmFalseEl a r = S.toList $ S.map (snd) (S.filter (\s -> fst s == a) (falseElements r))

getDomainOfFrmFalseEl :: (Eq b, Ord a) => b -> Relation a b -> [a]
getDomainOfFrmFalseEl b r = S.toList $ S.map (fst) (S.filter (\s -> snd s == b) (falseElements r))


-- || builds a List from a Relation.
toList :: Relation a b -> [(a, b)]
toList r = S.toList (elements r)

-- * constructors varieties

-- | Builds a Relation from a List of pairs.
fromListWithNames :: (Ord a, Ord b) => String -> String -> String -> [(a, b)] -> Relation a b
fromListWithNames s1 s2 s3 xs = setNames s1 s2 s3 $ fromList xs

fromDomRanFullRWNames :: (Ord a, Ord b) => String -> String -> String -> [a] -> [b] -> Relation a b
fromDomRanFullRWNames s1 s2 s3 ds rs = setNames s1 s2 s3 $ fromDomRanFullR ds rs


