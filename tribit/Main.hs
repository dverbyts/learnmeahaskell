module Main where
import qualified Pyramid
import qualified Transitions
import qualified System.Environment
{---   
 |Main module for tribits. Expects a single command line argument, which 
 |will be read as a string of 0's and 1's, from right to left, and will build 
 |and transform the resulting pyramid according to the tribits rules, while 
 |printing the requested step-wise information to the console.
 |
 |Unit tests are found in *_Test.hs source files and they require the 
 |Haskell unit testing package HUnit. The extra function "main_ghci" is provided 
 |in case you prefer to execute the code in a GHCI session. In that case, 
 |ghci_main expects one string argument which is the string of 0's and 1's that 
 |would have served as the command line argument, for example:
 |    > ghci Main
 |    > Prelude Main> Main.ghci_main "1011"
 |    1011
 |    1111
 |    1
 ---}
ghci_main :: String -> IO ()
ghci_main inputStr = do
    let pyramid = Pyramid.makePyramid inputStr 
    let steps = Transitions.returnSteps pyramid [] 
    mapM_ putStrLn $ (Pyramid.printPyramid pyramid):steps

main :: IO ()
main = do
    initialLine <- System.Environment.getArgs
    ghci_main $ initialLine !! 0
