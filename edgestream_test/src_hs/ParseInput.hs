module ParseInput where
import qualified Term

handleInputs :: IO [[Term.InputTriple]]
handleInputs = undefined
   args         <- getArgs
   content      <- readFile (args !! 0)
   let rawLines  = lines content
   return $ map parseLine rawLines

parseLine :: String -> [Term.InputTriple]
parseLine = undefined
