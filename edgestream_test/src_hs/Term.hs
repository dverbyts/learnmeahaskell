module Term where

type XPower      = Int
type YPower      = Int
type Coefficient = Int

-- Data structure to hold a single term.
data Term = Term Coefficient XPower YPower

isLike :: Term -> Term -> Bool
isLike (Term _ x1 y1) (Term _ x2 y2)
    | (x1 == x2) && (y1 == y2) = True
    | otherwise                = False

coefficient :: Term -> Coefficient
coefficient (Term c _ _) = c 

addCoefficients :: Term -> Term -> Term
addCoefficients (Term c1 x1 y1) (Term c2 _ _) = Term (c1+c2) x1 y1

termMultiply :: Term -> Term -> Term
termMultiply (Term c1 x1 y1) (Term c2 x2 y2) = Term (c1*c2) (x1+x2) (y1+y2)


    
 
