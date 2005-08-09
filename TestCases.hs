-- | some test cases to test the Boolean.* modules.

module Boolean.TestCases where

import Boolean.Algebra
import Debug.QuickCheck
import Prelude hiding(not, (&&), (||),and,or,any,all)
--import ArbitraryInstances

infix 4 ===

class Equality a where
    (===) :: a -> a -> Bool


instance Equality Bool where
    (===) = (==)

instance Equality Int where
    0 === 0 = True
    0 === _ = False
    _ === 0 = False
    _ === _ = True

instance (Equality a, Equality b) =>  Equality (a,b) where
    (x,y) === (x',y') = (x === x' && y === y')

--x === y = toBool x == toBool y

-- semi-boolean properties
prop_semi_and (x,y) = let z = x && y in z `elem` [x,y]
prop_semi_or (x,y) = let z = x || y in z `elem` [x,y]
prop_semi_b   (x,y) = (y && x) || x == x

-- we need equality for these to work
prop_notnot x = (not (not x)) === x
prop_true x = ((true && x) == x) && ((true || x) == true)
prop_false x = ((false || x) == x)
prop_false' x = ((false && x) == false)
prop_demorgan xs = (not (and xs)) === or (map not xs)
prop_demorgan' xs = (not (or xs)) === and (map not xs)

prop_truefalse true false x = ((true && x) == x) && ((true || x) == true) && ((false || x) == x) &&  ((false && x) == false)

main = do
    quickCheck (\(x::Bool) -> prop_notnot x)
    quickCheck (\(x::Int) -> prop_notnot x)
    quickCheck (\(x::Int) -> prop_true x)
    quickCheck (\(x::Int) -> prop_false x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_notnot x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_true x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_false x)
    quickCheck (\(x::(Int,(Bool,Int))) -> prop_false' x)
    quickCheck (\(x::[(Int,(Bool,Int))]) -> null x `trivial` prop_demorgan x)
    quickCheck (\(x::[(Int,(Bool,Int))]) -> null x `trivial` prop_demorgan' x)
    quickCheck $ prop_truefalse [3::Int] []
    quickCheck $ prop_truefalse (Just True) Nothing
    quickCheck $ prop_truefalse ((Right True),[3::Int]) (Left (), [])


