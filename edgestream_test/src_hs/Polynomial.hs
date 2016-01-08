module Polynomial where
import Term (isLike)
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

multiplyPair :: (Polynomial, Polynomial) -> Polynomial
multiplyPair (p1, p2) = polynomialMultiply p1 p2

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
processTerms = collectLikeTerms . filterNonZero

makePolynomial :: [Term.InputTriple] -> Polynomial
makePolynomial = map Term.makeTermFromTriple

-- TODO: add sorting by descending x and ascending y

