module BinRelationV3 where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Sq 

--
-- * Relation 
--

-- * Binary relation R on A B  - subset of A x B
-- type RelAnnot = (String, String, String) -- (relation name, left elem, right elem) 
-- type Relation a = (RelAnnot, [(a,a)]) 

type Names = [String]

data Relation a b  = Relation
  { elements      :: S.Set (a, b)
  , falseElements :: S.Set (a, b)
  , relatedBy     :: String
  , dname         :: String
  , rname         :: String
  } deriving (Show, Eq, Ord)
    
-- * Constructors 

-- | Builds an empty Relation 
empty :: Relation a b
empty = Relation S.empty S.empty "" "" ""

-- | Builds a Relation from a Single pair 
singleton :: a -> b -> Relation a b
singleton x y  = empty
  { elements      = S.singleton (x,y) 
  , falseElements   = S.empty
  }

-- | Builds a Relation from a List of pairs.
fromList :: (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList xs = empty
   { elements      = S.fromList xs
   , falseElements   = S.empty
   }
   
-- | Bulids a `Full` relation given Domain and Range 
-- Full Relation = Domain `cartesianProduct` Range
fromDomRanFullR :: (Ord a, Ord b) => [a] -> [b] -> Relation a b
fromDomRanFullR ds rs = Relation (xs) (S.empty) "" "" ""
             where xs = S.cartesianProduct (S.fromList ds) (S.fromList rs)

-- * Combinators

-- | Builds a Relation from the union of two relations: r and s 
--   union r s = L.nub (r ++ s)
union :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
union r s = copyNamesFromR r (Relation
   (S.union (elements r) (elements s))
   (S.union (falseElements  r) (falseElements  s)) "" "" "")

unions :: (Ord a, Ord b) => [Relation a b] -> Relation a b
unions []     = empty
unions (x:xs) = copyNamesFromR x $ foldl union empty xs
  
-- | Compose two relations: (left to right) 
--  compose r s = L.nub [(a, c) | (a, b) <- r, (b, c) <- s]
compose :: (Ord a, Ord b, Ord c) => Relation a b -> Relation b c -> Relation a c
compose r s = Relation 
      (listcompose (S.toList (elements r)   ) (S.toList (elements s)   ) )
      (listcompose (S.toList (falseElements r)) (S.toList (falseElements s)) )
       s1 s2 s3
    where listcompose s1 s2 = S.fromList [(a, c) | (a, b1) <- s1, (b2, c) <- s2, b1 == b2]
          [s1, s2, s3] = getcomposedNames r s

-- | Builds a Relation from the intersection of two relations: r and s 
intersection :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
intersection r s = copyNamesFromR r (Relation
   (S.intersection (elements r) (elements s))
   (S.intersection (falseElements  r) (falseElements  s)) "" "" "")

-- | Insert a relation x and y in the relation r
insert :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insert x y r = r {elements = S.insert (x, y) (elements r)}
 
-- |  Delete a relation element (a, b) from the relation.
delete :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
delete x y r = r {elements = S.delete (x, y) (elements r)}


-- * Functions 

sizeEl :: Relation a b -> Int
sizeEl r = S.size (elements r)

sizeflaseEl :: Relation a b -> Int
sizeflaseEl r = S.size (falseElements r)

getnElem:: Int -> Relation a b -> (a, b)
getnElem n r = S.elemAt n setElem
                     where setElem = elements r 

-- | set false elements

insertfalseEl :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insertfalseEl x y r = r {falseElements = S.insert (x, y) (falseElements r)}

deletefalseEl :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
deletefalseEl x y r = r {falseElements = S.delete (x, y) (falseElements r)}

setfalseElements :: (Ord a, Ord b) => [(a, b)] -> Relation a b -> Relation a b
setfalseElements nxs r = r { falseElements = S.fromList nxs }

-- | Access names

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

copyNamesFromR :: Relation c d -> Relation a b -> Relation a b 
copyNamesFromR r s = setNames (relatedBy r) (dname r) (rname r)  s

getcomposedNames :: Relation a b -> Relation b c -> [String]
getcomposedNames r s = [getRelatedBy r,  getDomainName r, getRangeName s]

getDomain :: Ord a => Relation a b -> [a]
getDomain r = S.toList (S.map (fst) (elements r))

getRange :: Ord b => Relation a b -> [b]
getRange r = S.toList (S.map (snd) (elements r))

getRangeOf :: (Eq a, Ord b) => a -> Relation a b -> [b]
getRangeOf a r = S.toList $ S.map (snd) (S.filter (\s -> fst s == a) (elements r))

getDomainOf :: (Eq b, Ord a) => b -> Relation a b -> [a]
getDomainOf b r = S.toList $ S.map (fst) (S.filter (\s -> snd s == b) (elements r))

getNthDomain :: Ord a => Int -> Relation a b -> a
getNthDomain n r = ((getDomain r) !! n)

getNthRange :: Ord b => Int -> Relation a b -> b
getNthRange n r = ((getRange r) !! n) 


-- builds a List from a Relation.
toList :: Relation a b -> [(a, b)]
toList r = S.toList (elements r)

-- * constructors varieties

-- | Builds a Relation from a List of pairs.
fromListWithNames :: (Ord a, Ord b) => String -> String -> String -> [(a, b)] -> Relation a b
fromListWithNames s1 s2 s3 xs = setNames s1 s2 s3 $ fromList xs

fromDomRanFullRWNames :: (Ord a, Ord b) => String -> String -> String -> [a] -> [b] -> Relation a b
fromDomRanFullRWNames s1 s2 s3 ds rs = setNames s1 s2 s3 $ fromDomRanFullR ds rs


