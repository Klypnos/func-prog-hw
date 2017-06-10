module Homework2 where

data Trie k a = Leaf a | Branch [(Maybe k, Trie k a)]
    deriving Show

empty' :: Trie k a
empty' = Branch []

insert' :: Eq k => [k] -> a -> Trie k a -> Trie k a
insert' [] a (Branch []) = Branch [(Nothing, Leaf a)] -- leaf addition step
insert' xs a (Branch []) = Branch [(Just (head xs), insert' (tail xs) a empty')] --straightforward addition part
insert' (x:xs) a (Branch t) = case t of
    [(Nothing, Leaf b)] -> Branch t
    [(Just b, rest)] -> if b == x then Branch [(Just b, insert' xs a rest)] else Branch ((Just b, rest):asd)
        where Branch asd = insert' (x:xs) a (Branch [])
    ((Just b, rest):ts) -> if b == x then Branch [(Just b, insert' xs a rest)] else Branch ((Just b, rest):asd)
        where Branch asd = insert' (x:xs) a (Branch ts)

fromList' :: Eq k => [([k], a)] -> Trie k a
fromList' xs = foldr (\x -> insert' (fst x) (snd x)) empty' xs

lookup' :: Eq k => [k] -> Trie k a -> Maybe a
lookup' [] (Branch [(Nothing, Leaf x)]) = Just x
lookup' (x:xs) (Branch t) = case t of
    [(Just t1, rest)] -> if t1==x then lookup' xs rest else Nothing
    ((Just t1,rest):ts) -> if t1==x then lookup' xs rest else lookup' (x:xs) (Branch ts)

delete' :: (Eq k , Eq a)=> [k] -> Trie k a -> Trie k a
delete' xs (Branch t)
    | length t==1 && lup == Nothing     = Branch []
    | length t==1                       = if sing then empty' else Branch [( fst $ head t, delete' (tail xs) $ snd $ head t)]
    | length t /= 1 && lup' == Nothing  = Branch ([head t] ++ asd)
    | otherwise                         = if sing then (Branch (tail t)) else  Branch (asd ++ (tail t))
        where lup  = lookup' xs (Branch t)
              sing = singularity' (Branch t)
              lup'  = lookup' xs (Branch [head t])
              Branch asd = delete' xs $ Branch (tail t)

singularity' :: Eq k => Trie k a -> Bool
singularity' (Branch [(Nothing, Leaf _)]) = True
singularity' (Branch xs)
    | length xs > 1 = False
    | otherwise = singularity' $ snd $ head xs
