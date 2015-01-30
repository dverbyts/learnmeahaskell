module DataStore where
import SessionId
import NormalizedQuery
import qualified Data.Map as M

type QueryTable = [(SessionId, NormalizedQuery)]
type ProductTable = [(SessionId, ProductId)]
type ResultTable = M.Map NormalizedQuery (M.Map ProductId Int)

join :: QueryTable -> ProductTable -> ResultTable
join [] pt = M.empty
join qt [] = M.empty
join qt pt = foldr fn M.empty pt  
    where qtHash = M.fromList qt
          fn nextFromPt acc = 
              let key = fst nextFromPt
                  val = snd nextFromPt
                  candidate = Map.lookup key qtHash
              in if Maybe.isJust candidate 
                  then M.insert key ((Maybe.fromJust candidate), val) acc
                  else acc 




join' :: QueryTable -> ProductTable -> ResultTable
join' qt pt = M.intersectionWith (,) (M.fromList qt) (M.fromList pt)


   
    






