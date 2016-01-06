module Polynomial where
import Term (isLike)
import qualified Term

type Polynomial = [Term.Term]

polynomialMultiply :: Polynomial -> Polynomial -> Polynomial
polynomialMultiply [] _ = []
polynomialMultiply _ [] = []
polynomialMultiply (p1:p1s) p2s = multiplyFirst ++ multiplyRest
    where multiplyFirst = [Term.termMultiply p1 p2 | p2 <- p2s]
          multiplyRest  = polynomialMultiply p1s p2s

collectLikeTerms :: Polynomial -> Polynomial
collectLikeTerms [] = []
collectLikeTerms (t1:ts) = (collectMatchesToFirstTerm : collectMatchesInTail)
    where matches                   = (t1:[t | t <- ts, t `isLike` t1])   
          notMatches                = [t | t <- ts, not $ t `isLike` t1] 
          collectMatchesToFirstTerm = collectLikeTerms' matches
          collectMatchesInTail      = collectLikeTerms notMatches 

collectLikeTerms' :: Polynomial -> Term.Term
collectLikeTerms' []     = Term.Term 0 0 0
collectLikeTerms' (t:ts) = foldl Term.addCoefficients t ts


