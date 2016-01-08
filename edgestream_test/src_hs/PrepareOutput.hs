module PrepareOutput where
import qualified Term
import qualified Polynomial
import Data.List (intercalate)

type SignString     = String
type BaseString     = String
type ExponentString = String

repBlank :: Int -> String
repBlank n = replicate n ' '

showSign :: Term.Sign -> (String, String)
showSign Term.Plus  = ("   ", " + ")
showSign Term.Minus = ("   ", " - ")

showCoefficient :: Term.Coefficient -> String
showCoefficient 1 = "" 
showCoefficient c = show c

showExponent :: Term.Exponent -> String
showExponent = showCoefficient

showPolynomial :: Polynomial.Polynomial -> String
showPolynomial [] = []
showPolynomial
showPolynomial p = fExp ++ rExp ++ "\n" ++ fBase ++ rBase ++ "\n"
    where (sp:sps)      = Polynomial.collectLikeTerms p
          (fExp, fBase) = showFirstTerm sp
          rests         = map showTerm sps
          rExp          = intercalate "" [rE | (rE, _) <- rests]
          rBase         = intercalate "" [rB | (_, rB) <- rests]

printPolynomial :: Polynomial.Polynomial -> IO ()
printPolynomial = putStr . showPolynomial


-- Helper function mainly for debugging and interactive printing.
printTerm :: Term.Term -> IO ()
printTerm t = do
    let (exp, base) = showTerm t
    putStrLn exp
    putStrLn base


showTerm :: Term.Term -> (ExponentString, BaseString)
showTerm (Term.Term _ 0 0 0) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign Term.Plus
          (eTail, bTail)   = (" ", "0")

showTerm (Term.Term _ 0 _ _) = ("", "")

showTerm (Term.Term s 1 0 0) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          (eTail, bTail)   = (" ", "1")

showTerm (Term.Term s c 0 0) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          cString          = showCoefficient c
          (eTail, bTail)   = (repBlank (length cString), cString)

showTerm (Term.Term s c 1 1) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          cString          = (showCoefficient c) ++ "xy"
          (eTail, bTail)   = (repBlank (length cString), cString)

showTerm (Term.Term s c 1 0) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          cString          = (showCoefficient c) ++ "x"
          (eTail, bTail)   = (repBlank (length cString), cString)

showTerm (Term.Term s c 0 1) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          cString          = (showCoefficient c) ++ "y"
          (eTail, bTail)   = (repBlank (length cString), cString)

showTerm (Term.Term s c x 0) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          expStr           = showExponent x
          expBlank         = repBlank (length expBlank)
          cString          = (showCoefficient c) ++ "x" ++ expBlank
          eString          = repBlank (length cString) ++ expStr
          (eTail, bTail)   = (eString, cString)

showTerm (Term.Term s c 0 y) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          expStr           = showExponent y
          expBlank         = repBlank (length expStr)
          cString          = (showCoefficient c) ++ "y" ++ expBlank
          eString          = repBlank (length cString) ++ expStr
          (eTail, bTail)   = (eString, cString)

showTerm (Term.Term s c x y) = (eFront ++ eTail, bFront ++ bTail)
    where (eFront, bFront) = showSign s
          coeffStr         = showCoefficient c
          bStringFront     = coeffStr ++ "x"
          expFrontBlanks   = repBlank (length bStringFront)
          xExpStr          = showExponent x
          xExpBlank        = repBlank (length xExpStr)
          yExpStr          = showExponent y
          yExpBlank        = repBlank (length yExpStr)
          bStringBack      = xExpBlank ++ "y" ++ yExpBlank
          eString          = expFrontBlanks ++ xExpStr ++ " " ++ yExpStr
          bString          = bStringFront ++ bStringBack
          (eTail, bTail)   = (eString, bString)


showFirstTerm :: Term.Term -> (ExponentString, BaseString)
showFirstTerm term@(Term.Term s c x y) = (expSign ++ expRest, sign ++ rest)
    where sign                  = if s == Term.Plus then "" else "-"
          expSign               = if s == Term.Plus then "" else " " 
          (expRest', baseRest') = showTerm term
          rest                  = drop 3 $ baseRest'
          expRest               = drop 3 $ expRest'
