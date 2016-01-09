module Polynomial where

import Term (isLike)
import Data.List (sortBy)
import qualified Term

type Polynomial  = [Term.Term]

filterNonZero :: Polynomial -> Polynomial
filterNonZero = filter Term.nonZero

polynomialMultiply :: Polynomial -> Polynomial -> Polynomial
polynomialMultiply [] _ = []
polynomialMultiply _ [] = []
polynomialMultiply (p1:p1s) p2s = multiplyFirst ++ multiplyRest
    where multiplyFirst = [Term.termMultiply p1 p2 | p2 <- p2s]
          multiplyRest  = polynomialMultiply p1s p2s

multiplyPair :: [Polynomial] -> Polynomial
multiplyPair [p1, p2] = processTerms $ polynomialMultiply p1 p2
multiplyPair _        = error "multiplyPair can only process 2-element List."

collectLikeTerms :: Polynomial -> Polynomial
collectLikeTerms [] = []
collectLikeTerms (t1:ts) = (collectMatchesToFirstTerm : collectMatchesInTail)
    where matches                   = (t1:[t | t <- ts, t `isLike` t1])   
          notMatches                = [t | t <- ts, not $ t `isLike` t1] 
          collectMatchesToFirstTerm = collectLikeTerms' matches
          collectMatchesInTail      = collectLikeTerms notMatches 

collectLikeTerms' :: Polynomial -> Term.Term
collectLikeTerms' []     = Term.Term Term.Plus 0 0 0
collectLikeTerms' (t:ts) = foldl Term.termAdd t ts

processTerms :: Polynomial -> Polynomial
processTerms = (sortBy Term.sortTerms) . collectLikeTerms . filterNonZero

