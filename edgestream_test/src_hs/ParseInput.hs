module ParseInput where

import qualified Term
import qualified Polynomial
import qualified Text.Regex.Posix as RE
import qualified PrepareOutput as PO

import Data.List (intercalate)
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)

handleInputs :: IO [Polynomial.Polynomial]
handleInputs = do
   args         <- getArgs
   content      <- readFile (args !! 0)
   let rawLines  = lines content
   return $ map parseLine rawLines

parseLine :: String -> Polynomial.Polynomial
parseLine s = inputTriples 
    where p1           = "^[-]?[0-9]*x?[0-9]*y?[0-9]*"
          p2           = "[-,+][0-9]*x?[0-9]*y?[0-9]*"
          p3           = "^[-]?[0-9]*y?[0-9]*x?[0-9]*"
          p4           = "[-,+][0-9]*y?[0-9]*x?[0-9]*"
          pattern      = "(" ++ intercalate "|" [p1, p2, p3, p4] ++ ")"
          rawParsed    = (RE.getAllTextMatches (s =~ pattern)) :: [String]
          inputTriples = map parseTerm rawParsed

parseTerm :: String -> Term.Term
parseTerm s = Term.Term sign c xE yE
    where negCoeff  = getSubmatch $ patternHelper "^-([0-9]*)" s
          posCoeff  = getSubmatch $ patternHelper "^[+]?([0-9]*)" s
          xExp      = getSubmatch $ patternHelper "x([0-9]*)" s
          yExp      = getSubmatch $ patternHelper "y([0-9]*)" s
          (sign, c) = case (negCoeff, posCoeff) of
                         ("", c) -> (Term.Plus, (read c) :: Int)
                         (c, "") -> (Term.Minus, (read c) :: Int)
          xE        = if xExp == "" then 1 else (read xExp) :: Int
          yE        = if yExp == "" then 1 else (read yExp) :: Int


patternHelper :: String -> String -> [String]
patternHelper pattern s = (RE.getAllTextSubmatches (s =~ pattern)) :: [String]

getSubmatch :: [String] -> String
getSubmatch [] = ""
getSubmatch xs = last xs



