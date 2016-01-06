module PrepareOutput where
import qualified Term
import qualified Polynomial
import Data.List (intercalate)

type SignString     = String
type BaseString     = String
type ExponentString = String

showSign :: Int -> String
showSign c = if c < 0 then "-" else "" 

showModOne :: Int -> String
showModOne 1 = "" 
showModOne c 
    | c < 0     = (showModOne . abs) c
    | otherwise = show c

showTerm :: Term.Term -> (SignString, ExponentString, BaseString)

-- Case of printing zero.
showTerm (Term.Term 0 _ _) = ("", "", "")

-- Case of printing single constants 1 / -1.
showTerm (Term.Term    1 0 0) = ("", "", "1")
showTerm (Term.Term (-1) 0 0) = ("-", "", "1")

-- Case of printing a non-one constant
showTerm (Term.Term c 0 0) = (showSign c, "", showModOne c)

-- Case of first order terms
showTerm (Term.Term c 1 1) = (showSign c, "", showModOne c ++ "xy")
showTerm (Term.Term c 1 0) = (showSign c, "", showModOne c ++ "x")
showTerm (Term.Term c 0 1) = (showSign c, "", showModOne c ++ "y")

-- Case of generic terms with only powers of x
showTerm (Term.Term c x 0) = (sign, whitespace ++ sx, sc ++ "x")
    where [sc, sx]   = map showModOne [c, x] 
          sign       = showSign c
          whitespace = replicate (length sign + length sc + 1) ' '

-- Case of generic terms with only powers of y
showTerm (Term.Term c 0 y) = (sign, whitespace ++ sy, sc ++ "y")
    where [sc, sy]   = map showModOne [c, y] 
          sign       = showSign c
          whitespace = replicate (length sign + length sc + 1) ' '

-- Case of fully generic terms.
showTerm (Term.Term c x y) = (sign, exponentString, baseString)
    where [sc, sx, sy]   = map showModOne [c, x, y]
          sign           = showSign c
          xWhitespace    = replicate (length sign + length sc + 1) ' '
          yWhitespace    = " "
          baseWhitespace = replicate (length sx) ' '
          trailingWS     = replicate (length sy) ' '
          baseString     = sc ++ "x" ++ baseWhitespace ++ "y" ++ trailingWS
          exponentString = xWhitespace ++ sx ++ yWhitespace ++ sy
----- End showTerm
-----


assembleLine :: [(SignString, ExponentString, BaseString)] -> String
assembleLine [] = ""
assembleLine ((sign, exp, base):ts) = exponentStr ++ baseStr
    where signedBases = [(if s == "-" then " - " else " + ") ++ b 
                         | (s, _, b) <- ts, b /= ""]
          signedExps  = [(if s == "-" then "  " else "   ") ++ e 
                         | (s, e, b) <- ts, b /= ""]
          tailOfBases = intercalate "" signedBases
          tailOfExps  = intercalate "" signedExps 
          headOfBases = sign ++ base
          exponentStr = exp ++ tailOfExps ++ "\n"
          baseStr     = headOfBases ++ tailOfBases ++ "\n"


showPolynomial :: Polynomial.Polynomial -> String
showPolynomial p = assembleLine $ map showTerm p          

printPolynomial :: Polynomial.Polynomial -> IO ()
printPolynomial = putStr . showPolynomial


-- Helper function mainly for debugging and interactive printing.
printTerm :: Term.Term -> IO ()
printTerm t = do
    let (sign, exp, base) = showTerm t
    putStrLn exp
    putStrLn (sign ++ base)
