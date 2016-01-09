module ParseInput where

import qualified Term
import qualified Polynomial as P
import qualified PrepareOutput as PO
import qualified Text.Regex.Posix as RE

import Data.List (intercalate)
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)

handleInputs :: IO [P.Polynomial]
handleInputs = do
   (fileToRead:_) <- getArgs
   content        <- readFile fileToRead
   let rawLines    = lines content
   return $ map parseLine rawLines

parseLine :: String -> P.Polynomial
parseLine [] = []
parseLine s
    | head s == '#' = []
    | otherwise = term
        where p1        = "^[-]?[0-9]*x?[0-9]*y?[0-9]*"
              p2        = "[-,+][0-9]*x?[0-9]*y?[0-9]*"
              p3        = "^[-]?[0-9]*y?[0-9]*x?[0-9]*"
              p4        = "[-,+][0-9]*y?[0-9]*x?[0-9]*"
              pattern   = "(" ++ intercalate "|" [p1, p2, p3, p4] ++ ")"
              rawParsed = (RE.getAllTextMatches (s =~ pattern)) :: [String]
              term      = map parseTerm rawParsed

parseTerm :: String -> Term.Term
parseTerm s = Term.makeTerm coeff xE yE
    where negCoeff = getCoeffSubmatch $ negCoeff' s
          posCoeff = getCoeffSubmatch $ posCoeff' s
          xExp     = getExponentSubmatch $ xExp' s
          yExp     = getExponentSubmatch $ yExp' s
          coeff    = if negCoeff == ""
                         then read posCoeff :: Int
                         else read negCoeff :: Int
          xE       = read xExp :: Int
          yE       = read yExp :: Int

patternHelper :: String -> String -> [String]
patternHelper pattern s = (RE.getAllTextSubmatches (s =~ pattern)) :: [String]

negCoeff' :: String -> [String]
negCoeff' = patternHelper "^(-[0-9]*)[x,y]?"

posCoeff' :: String -> [String]
posCoeff' = patternHelper "^[+]?([0-9]*)[x,y]?"

xExp' :: String -> [String]
xExp' = patternHelper "x([0-9]*)"

yExp' :: String -> [String]
yExp' = patternHelper "y([0-9]*)"

getExponentSubmatch :: [String] -> String
getExponentSubmatch [] = "0"
getExponentSubmatch xs = case xs of
    ["", ""]  -> "0"
    ["x", _]  -> "1"
    ["y", _]  -> "1"
    [_, s]    -> s
    _         -> error "Incorrect number of regex matches for exponent match."

getCoeffSubmatch :: [String] -> String
getCoeffSubmatch [] = ""
getCoeffSubmatch xs = case xs of
    ["", ""]   -> ""
    ["x", _]   -> "1"
    ["y", _]   -> "1"
    ["+x", _]  -> "1"
    ["+y", _]  -> "1"
    ["+1x", _] -> "1"
    ["+1y", _] -> "1"
    ["-x", _]  -> "-1"
    ["-y", _]  -> "-1"
    ["-1x", _] -> "-1"
    ["-1y", _] -> "-1"
    [_, s]     -> s
    _          -> error "Incorrect number of regex matches for coefficient match."


-- Recipe for taking chunks of two, to avoid needing Data.List.Split. 
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n l
  | n > 0     = (take n l) : (groupsOf n (drop n l))
  | otherwise = error "Cannot form groupsOf with negative n"
