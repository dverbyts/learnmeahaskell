module KDTree (KDTree(KDEmptyTree, KDNode), fromList) where
import Data.List (sortBy)

type KDId = Int
type KDDepth = Int
type KDCoordinate = [Float]
type KDPoint = (KDId, KDCoordinate)
type KDPointList = [KDPoint]
data KDTree = KDEmptyTree 
            | KDNode {idVal      :: KDId,
                      location   :: KDCoordinate,
                      splitVal   :: Float,
                      splitAxis  :: Int,
                      leftChild  :: KDTree, 
                      rightChild :: KDTree}
            deriving (Show, Eq)

type KDLine = (Int, Float)
type KDCircle = (KDCoordinate, KDCoordinate, Float)

    
-- Recursively create KD Tree from an input.
fromList :: KDPointList -> KDTree
fromList = fromList' 0 


{---
 | Hidden helper functions.
 ---}

-- Helper to sort [KDPoint] by a certain axis into the KDCoordinate that is the
-- snd component of the KDPoint.
axisSort :: KDDepth -> KDPoint -> KDPoint -> Ordering
axisSort depth pointA pointB = 
    ((snd pointA) !! depth) `compare` ((snd pointB) !! depth)

-- Helper function that recursively makes the tree. At each level of recursion,
-- if supplies a depth value that determines which axis to use for splitting.
fromList' :: KDPointList -> KDDepth -> KDTree
fromList' [] _ = KDEmptyTree
fromList' depth pointList =
    let k      = (length . snd . head) pointList
        n      = length pointList
        m      = max 0 (n `div` 2) -- Location of median in sorted list.
        axis   = depth `mod` k -- Which axis to sort by
        sorted = sortBy (axisSort axis) pointList -- Points sorted by axis val.
        median = sorted !! m -- Point which has median axis value.
        front  = take (m) sorted -- Points "to the left" of median.
        back   = drop (m+1) sorted -- Points "to the right" of median.
    in KDNode {idVal      = fst median,
               location   = snd median, 
               splitVal   = median !! axis,
               splitAxis  = axis,
               leftChild  = fromList' (depth + 1) front,
               rightChild = fromList' (depth + 1) back}



