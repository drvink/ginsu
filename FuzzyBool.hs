module Boolean.FuzzyBool where


import Boolean.Algebra
import Ratio


type Fuzzy = Fuzz Rational

newtype Fuzz a = Fuzz { fromFuzz :: a }
    deriving(Ord,Eq,Enum,Bounded,Show,Read)


instance Ord a => SemiBooleanAlgebra (Fuzz a) where
    a && b = min a b
    a || b = max a b

instance (Ord a, Num a) => BooleanAlgebra (Fuzz a) where
    true = Fuzz 1
    false = Fuzz 0
    not (Fuzz x) = Fuzz (1 - x)
    and xs = minimum (true:xs)
    or xs = maximum (false:xs)
    

avg (Fuzz a) (Fuzz b) = (a + b) / 2 
average [] = error "average of empty list"   -- should this degenerate into something
average xs = f xs 0 0 where
    f [] n m = Fuzz (n / m) 
    f (Fuzz x:xs) n m = a `seq` b `seq` f xs a b   where
        a = n + x
        b = m + 1
