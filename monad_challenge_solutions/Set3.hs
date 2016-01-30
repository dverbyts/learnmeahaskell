{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}
module Set3 where
import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) ys = (map ((,) x) ys) ++ (allPairs xs ys)

data Card = Card Int String

instance Show Card where
    show (Card rank suit) = (show rank) ++ (stripQuotes $ show suit)
        where stripQuotes = (filter . flip notElem) "\"" 

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) ys = (map (Card x) ys) ++ (allCards xs ys) 

allPerms :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms _ [] _ = []
allPerms _ _ [] = []
allPerms f (x:xs) ys = (map (f x) ys) ++ (allPerms f xs ys)

allPairs2 = allPerms (,)
allCards2 = allPerms Card

allPerms3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3 _ [] _ _ = []
allPerms3 _ _ [] _ = []
allPerms3 _ _ _ [] = []
allPerms3 f (x:xs) ys zs = 
    concat (map (\g -> map g zs) (map (f x) ys)) ++ 
    (allPerms3 f xs ys zs)

permStep :: [a -> b] -> [a] -> [b]
permStep [] _ = []
permStep _ [] = []
permStep (f:fs) as = (map f as) ++ (permStep fs as)

allPerms' :: (a -> b -> c) -> [a] -> [b] -> [c]
allPerms' f as bs = (map f as) `permStep` bs

allPerms3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allPerms3' f as bs cs = (map f as) `permStep` bs `permStep` cs

