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
parseTerm s = Term.makeTerm coeff xE yE
    where negCoeff  = getCoeffSubmatch $ negCoeff' s
          posCoeff  = getCoeffSubmatch $ posCoeff' s
          xExp      = getExponentSubmatch $ xExp' s
          yExp      = getExponentSubmatch $ yExp' s
          coeff     = if negCoeff == ""
                          then read posCoeff :: Int
                          else read negCoeff :: Int
          xE        = read xExp :: Int
          yE        = read yExp :: Int


patternHelper :: String -> String -> [String]
patternHelper pattern s = (RE.getAllTextSubmatches (s =~ pattern)) :: [String]

negCoeff' :: String -> [String]
negCoeff' = patternHelper "^(-[0-9]*)[x,y]?"

posCoeff' :: String -> [String]
posCoeff' = patternHelper "^[+]?([0-9]*)[x,y]?"

xExp' :: String -> [String]
xExp'     = patternHelper "x([0-9]*)"

yExp' :: String -> [String]
yExp'     = patternHelper "y([0-9]*)"

getExponentSubmatch :: [String] -> String
getExponentSubmatch [] = "0"
getExponentSubmatch xs = case xs of
    ["", ""]  -> "0"
    ["x", _]  -> "1"
    ["y", _]  -> "1"
    [_, s]    -> s

getCoeffSubmatch :: [String] -> String
getCoeffSubmatch [] = ""
getCoeffSubmatch xs = case xs of
    ["", ""]   -> ""
    ["x", _]   -> "1"
    ["y", _]   -> "1"
    ["-x", _]  -> "-1"
    ["-y", _]  -> "-1"
    ["-1x", _] -> "-1"
    ["-1y", _] -> "-1"
    [_, s]     -> s



