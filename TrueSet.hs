module Boolean.TrueSet(
    TrueSet,
    fromList,
    member, 
    empty,
    full,
    single,
    insert,
    delete,
    unions,
    union,
    intersection,
    intersections,
    difference,
    (\\)
    ) where

import Prelude hiding(not,(&&),(||),and,or)
import qualified Set 
import Boolean.Algebra

infixl 9 \\


data Ord a => TrueSet a = EverythingBut (Set a) | NothingBut (Set a)

instance BooleanAlgebra (TrueSet a) where
    not (EverythingBut s) = NothingBut s
    not (NothingBut s) = EverythingBut s
    and = intersections
    or = unions
    true = EverythingBut empty
    false = NothingBut empty

    EverythingBut x && EverythingBut y = EverythingBut (x `Set.union` y)
    NothingBut x && NothingBut y = NothingBut (x `Set.intersection` y)
    EverythingBut x && NothingBut y = NothingBut (y Set.\\ x)
    NothingBut x && EverythingBut y = NothingBut (x Set.\\ y)
    EverythingBut x || EverythingBut y = EverythingBut (x `Set.intersection` y)
    NothingBut x || NothingBut y = NothingBut (x `Set.union` y)
    EverythingBut x || NothingBut y = EverythingBut (x Set.\\ y)
    NothingBut x || EverythingBut y = EverithingBut (y Set.\\ x)


fromList = NothingBut . Set.fromList
member x (NothingBut s) = Set.member x s
member x (EverythingBut s) = not (Set.member x s)

empty = false
full = true
single = NothingBut . Set.single
insert x (NothingBut s) = NothingBut (Set.insert x s)
insert x (EverythingBut s) = EverythingBut (Set.delete x s)
delete x (NothingBut s) = NothingBut (Set.delete x s)
delete x (EverythingBut s) = EverythingBut (Set.insert x s)

unions xs = foldlStrict (||) false xs
intersections xs = foldlStrict (&&) true xs

foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)

union x y = x || y
intersection x y = x && y
difference x y = x && not y
m1 \\ m2 = difference m1 m2
