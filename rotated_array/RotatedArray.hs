module RotatedArray where
import qualified Data.List as List

data RotatedArray = RotatedArray [Int] deriving (Show, Eq)

fromList :: Int -> [Int] -> RotatedArray
fromList n xs = rotateBy n $ RotatedArray $ List.sort xs

rotateBy :: Int -> RotatedArray -> RotatedArray 
rotateBy n orig@(RotatedArray xs) 
    | null xs   = orig
    | n >= len  = rotateBy (mod n len) orig
    | otherwise = RotatedArray $ (drop offset xs) ++ (take offset xs)   
    where len = length xs
          offset = len - n

getRotation :: RotatedArray -> Int
getRotation (RotatedArray []) = 0
getRotation (RotatedArray xs) =
    let hh = head xs
        ll = last xs
        md = length xs `div` 2
        mm = xs !! (max 0 $ md - 1)
    in case () of
    _ | hh <= ll  -> 0
      | otherwise -> if hh <= mm
          then md + getRotation (RotatedArray (drop md xs))
          else getRotation (RotatedArray (take md xs))

