module BDD
( BDD(..)
, constructBDD
, prettyPrintBDD
) where

import Formula

import Data.Tree
import Data.Tree.Pretty

type Label = String
data BDD = Fork BDD Label BDD | Leaf Bool
  deriving Show

type BDDOrdering = [String]

data Operation = OpAND | OpOR | OpXOR

getOperation :: Operation -> Bool -> Bool -> Bool
getOperation OpAND = (&&)
getOperation OpOR = (||)
getOperation OpXOR = \b1 b2 -> (b1 && (not b2)) || ((not b1) && b2) 


lessThan :: BDDOrdering -> Label -> Label -> Bool
lessThan [] _ _ = error "invalid BDDOrdering"
lessThan (o:os) l1 l2 = if (o == l1) then True
                        else if (o == l2) then False
                        else lessThan os l1 l2

mergeOrderedBDDs :: BDDOrdering -> BDD -> BDD -> Operation -> BDD
mergeOrderedBDDs _ (Leaf b1) (Leaf b2) op = Leaf (getOperation op b1 b2)
mergeOrderedBDDs o (Leaf b1) (Fork l2 p2 r2) op = (Fork (mergeOrderedBDDs o (Leaf b1) l2 op) p2 (mergeOrderedBDDs o (Leaf b1) r2 op))
mergeOrderedBDDs o (Fork l1 p1 r1) (Fork l2 p2 r2) op
    | p1 == p2 = Fork (mergeOrderedBDDs' l1 l2) p1 (mergeOrderedBDDs' r1 r2)
    | lessThan o p1 p2 = Fork (mergeOrderedBDDs' l1 (Fork l2 p2 r2)) p1 (mergeOrderedBDDs' r1 (Fork l2 p2 r2))
    | otherwise = mergeOrderedBDDs' (Fork l2 p2 r2) (Fork l1 p1 r1)
  where mergeOrderedBDDs' t1 t2 = mergeOrderedBDDs o t1 t2 op
mergeOrderedBDDs o t1 t2 op = mergeOrderedBDDs o t2 t1 op -- swap trees




constructBDD :: BDDOrdering -> Formula -> BDD
constructBDD _ (Var s) = Fork (Leaf False) s (Leaf True)
constructBDD _ (Const b) = Leaf b
constructBDD o (NOT f) = mergeOrderedBDDs o (constructBDD o f) (Leaf True) OpXOR
constructBDD o (OR f1 f2) = mergeOrderedBDDs o (constructBDD o f1) (constructBDD o f2) OpOR
constructBDD o (AND f1 f2) = mergeOrderedBDDs o (constructBDD o f1) (constructBDD o f2) OpAND


convert :: BDD -> Tree String
convert (Leaf b) = Node (show b) []
convert (Fork l s r) = Node s [convert l, convert r]

prettyPrintBDD :: BDD -> String
prettyPrintBDD = drawVerticalTree . convert
