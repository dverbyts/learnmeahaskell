module RotatedArray where
import qualified Data.List as List

data RotatedArray = RotatedArray [Int] deriving (Show, Eq)

fromList :: Int -> [Int] -> RotatedArray
fromList n xs = rotateBy n $ RotatedArray $ List.sort xs

rotateBy :: Int -> RotatedArray -> RotatedArray 
rotateBy 0 x = x
rotateBy _ orig@(RotatedArray []) = orig
rotateBy n (RotatedArray xs) =
    let len    = length xs
        offset = len - (n `mod` len)
    in RotatedArray $ (drop offset xs) ++ (take offset xs)    

getRotation :: RotatedArray -> Int
getRotation (RotatedArray []) = 0
getRotation (RotatedArray xs)  
    | firstElem <= last xs = 0
    | firstElem <= midElem = mid + (getRotation $ RotatedArray (drop mid xs))
    | otherwise            = getRotation $ RotatedArray (take mid xs)
    where firstElem = head xs
          mid       = length xs `div` 2
          midElem   = xs !! max 0 (mid - 1)

