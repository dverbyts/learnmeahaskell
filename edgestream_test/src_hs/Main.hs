module Main where
import qualified Polynomial as P 
import qualified ParseInput as PI
import qualified PrepareOutput as PO


main :: IO ()
main = do
    polynomials         <- PI.handleInputs
    let pairsToMultiply  = zip (init polynomials) (tail polynomials) 
    let results          = map P.multiplyPair pairsToMultiply

    mapM_ PO.printPolynomial results
