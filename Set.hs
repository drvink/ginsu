
-- | This creates the boolean algebra of sets from any base boolean algebra.
-- note that the sets created are 'true' sets in the mathematical sense, not
-- the usual programatic aproximation.
-- 
-- A generalized set can be thought of as a map from keys to boolean values.
-- perhaps the 'map with default' should be seperated out?

module Boolean.Set where

-- needs 'Map' from DData
import Data.Map as Map hiding(member) 

data Set k v = Set v (Map k v) 

instance Functor (Set k) where
    fmap f (Set v map) = Set (f v) (Map.map f map)



instance SemiBooleanAlgebra v => SemiBooleanAlgebra (Set k v) where
    (&&) = combine (&&) 
    (||) = combine (||)

instance BooleanAlgebra v => BooleanAlgebra (Set k v) where
    not x = fmap not x
    true = Set true Map.empty
    false = Set false Map.empty
    xor = combine xor


-- TODO 
-- this is very inefficient, but we need a generalized unionWith to do it properly

combine :: (v -> v -> v) -> Set k v -> Set k v -> Set k v
combine f a@(Set da mapa) b@(Set db mapb) = Set (f da db) (Map.fromList $ map g $  keys mapa ++ keys mapb) where
    g k = (k,f (member k a) (member k b)) 

empty = false
single x (Set d map) = Set d (Map.insert x true map)
insert x v (Set d map)  = Set d (Map.insert x v map)

member :: k -> Set k v  -> v
member k (Set d map) = case Map.lookup k map of
    Just x -> x
    Nothing -> d


