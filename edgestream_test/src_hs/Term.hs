module Term where

type Exponent    = Int
type Coefficient = Int
type InputTriple = (Coefficient, Exponent, Exponent)

data Sign = Plus | Minus deriving (Eq, Show)

-- Data structure to hold a single term.
data Term = Term Sign Coefficient Exponent Exponent deriving (Show)

isLike :: Term -> Term -> Bool
isLike (Term _ _ x1 y1) (Term _ _ x2 y2)
    | (x1 == x2) && (y1 == y2) = True
    | otherwise                = False

nonZero :: Term -> Bool
nonZero (Term _ 0 _ _) = False
nonZero _              = True

getSign :: Term -> Sign
getSign (Term s _ _ _) = s

getCoefficient :: Term -> Coefficient
getCoefficient (Term _ c _ _) = c 

termAdd :: Term -> Term -> Term
termAdd (Term s1 c1 x1 y1) (Term s2 c2 _ _) = case (s1, s2) of
    (Plus, Plus)   -> Term Plus (c1+c2) x1 y1
    (Plus, Minus)  -> Term (if c1 > c2 then Plus else Minus) (abs (c1-c2)) x1 y1
    (Minus, Plus)  -> Term (if c1 > c2 then Minus else Plus) (abs (c2-c1)) x1 y1
    (Minus, Minus) -> Term Minus (c1+c2) x1 y1

termMultiply :: Term -> Term -> Term
termMultiply (Term s1 c1 x1 y1) (Term s2 c2 x2 y2) = case (s1, s2) of
    (Plus, Plus)   -> Term Plus (c1*c2) (x1+x2) (y1+y2)
    (Plus, Minus)  -> Term Minus (c1*c2) (x1+x2) (y1+y2)
    (Minus, Plus)  -> Term Minus (c1*c2) (x1+x2) (y1+y2)
    (Minus, Minus) -> Term Plus (c1*c2) (x1+x2) (y1+y2)

makeTerm :: Int -> Int -> Int -> Term
makeTerm c x y = Term.Term sign (abs c) x y
    where sign = if c < 0 then Minus else Plus 

makeTermFromTriple :: InputTriple -> Term
makeTermFromTriple (c, x, y) = makeTerm c x y


    
 
