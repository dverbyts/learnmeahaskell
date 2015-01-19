module RotatedArray where
import Data.List (sort)
import qualified Data.Vector as V

data RotatedArray = RotatedArray (V.Vector Int) deriving (Show, Eq)

fromList :: Int -> [Int] -> RotatedArray
fromList n xs = let srtd = sort xs
                    fn   = \i -> srtd !! i
                    vec  = V.generate (length xs) fn
                in rotateBy n $ RotatedArray vec

rotateBy :: Int -> RotatedArray -> RotatedArray 
rotateBy 0 x = x
rotateBy n original@(RotatedArray xs)
    | (V.null xs) = original
    | otherwise = let len    = V.length xs
                      offset = len - (n `mod` len)
                  in RotatedArray $ (V.drop offset xs) V.++ (V.take offset xs)

getRotation :: RotatedArray -> Int
getRotation (RotatedArray xs) 
    | (V.null xs)          = 0 
    | fstElem <= V.last xs = 0
    | fstElem <= midElem   = mid + (getRotation $ RotatedArray (V.drop mid xs))
    | otherwise            = getRotation $ RotatedArray (V.take mid xs)
    where fstElem = V.head xs
          mid     = V.length xs `div` 2
          midElem = xs V.! (max 0 (mid - 1))

