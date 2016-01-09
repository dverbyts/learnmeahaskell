module Main where
import qualified Polynomial as P 
import qualified ParseInput as PI
import qualified PrepareOutput as PO


main :: IO ()
main = do
    parsedPolynomials   <- PI.handleInputs
    let polynomials      = init parsedPolynomials -- drop last which must be #
    let pairsToMultiply  = PI.groupsOf 2 polynomials
    let results          = map P.multiplyPair pairsToMultiply

    mapM_ PO.printPolynomial results
