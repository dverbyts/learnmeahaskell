module Pyramid where
import qualified Cell
import qualified Data.List

--------------------------------------------
-- Pyramid data type and helper functions --
--------------------------------------------

{-
 Pyramid is the core data structure. It's recursively defined as four sub-
 pyramids, which are considered ordered 0-1-2-3 like the problem statement.
 The functions that manipulate them embody the logic for handling the third
 pyramid "upside down" and ensuring things are written correctly. It
 bottoms out with a cell defined in the Cell module.
 -}
data Pyramid
    = Atomic Cell.Cell
    | Composite Pyramid Pyramid Pyramid Pyramid 
    deriving (Eq, Show)
                       
-- Reports the total number of elements in a pyramid.
getLength :: Pyramid -> Int
getLength (Atomic c1) = 1
getLength (Composite p1 p2 p3 p4) = 4 * (getLength p1)

-- Reports the height (number of rows) in a pyramid.
getHeight :: Pyramid -> Int
getHeight (Atomic c1) = 1
getHeight (Composite p1 p2 p3 p4) = 2 * (getHeight p1)
       
-- Retrieves a row of Cell values from a pyramid, indexed 1 through
-- the height of the pyramid. Reports the cells in a list.
getRow :: Pyramid -> Int -> [Cell.Cell]
getRow (Atomic c) row = [[c]] !! (row - 1)
getRow (Composite (Atomic c1) (Atomic c2) (Atomic c3) (Atomic c4)) row =
    [[c1], [c2, c3, c4]] !! (row - 1)
    
-- Get rows recursively, paying special attention to adjust the row sought when
-- looking in the upside down third pyramid.
getRow (Composite p1 p2 p3 p4) row = 
    let hh = getHeight p1
    in if row <= hh
        then getRow p1 row      
        else let r  = row - hh     
                 r' = hh - r + 1
             in (getRow p2 r) ++ (getRow p3 r') ++ (getRow p4 r)    
----- End getRow
                
-- Constructs a new pyramid from an old pyramid, in which a given new row of
-- Cells will be used as values for the specified row. Works recursively like
-- getRow to ensure offsets are right for upside down triangles.
putRow :: Pyramid -> Int -> [Cell.Cell] -> Pyramid                
putRow (Atomic c) row values
    | (row == 1) && (length values == 1) = Atomic $ head values
    | otherwise = error "Only assign single value to first row of Atomic."
                  
-- Place a row in a pyramid with 4 cells.
putRow (Composite (Atomic c1) 
                  (Atomic c2) 
                  (Atomic c3) 
                  (Atomic c4)) row values
    -- Inserting the top row.
    | (row == 1) && (length values == 1) = (Composite (Atomic (values !! 0)) 
                                                      (Atomic c2) 
                                                      (Atomic c3) 
                                                      (Atomic c4))
    -- Inserting the bottom row.
    | (row == 2) && (length values == 3) = (Composite (Atomic c1) 
                                                      (Atomic (values !! 0)) 
                                                      (Atomic (values !! 1)) 
                                                      (Atomic (values !! 2)))
    | otherwise = error "Only assign single (row 1) or triple (row 2) values."
                  
-- If putting a row in a composite pyramid, check if it's the upper half or the
-- lower half and make recursive calls as needed.
putRow (Composite p1 p2 p3 p4) row values = 
    let hh = getHeight p1
        
    -- If putting a row in the top half, recursive call on top pyramid.
    in if row <= hh
        then Composite (putRow p1 row values) p2 p3 p4
             
        -- Otherwise, calculate row offsets for three bottom subpyramids and     
        -- and obtain the associated values for three further recursive row     
        -- placements. Note: evil nested let statement. Candidate for refactor.
        else let r        = row - hh     
                 r'       = hh - r + 1
                 sidesLen = length $ getRow p2 r
                 middlLen = length $ getRow p3 r'
                 p2vals   = take sidesLen values
                 p3vals   = take middlLen (drop sidesLen values)
                 p4vals   = take sidesLen (drop (sidesLen + middlLen) values)
             in (Composite p1    
                           (putRow p2 r  p2vals)
                           (putRow p3 r' p3vals) 
                           (putRow p4 r  p4vals))
----- End putRow

-- If a Pyramid has constant-valued cells, report the constant (wrapped in
-- Maybe) otherwise report Nothing.
getConst :: Pyramid -> Maybe Cell.Cell
getConst (Atomic c) = Just c
getConst (Composite p1 p2 p3 p4) =
    let x:xs = map getConst [p1, p2, p3, p4]
    in if (Data.List.all (== x) xs) 
        then x
        else Nothing
-----
-----

-----------------------------------------------
-- Reader/Printer to convert to/from String. --
-----------------------------------------------
odds = [1,3..]  -- Odds, only permissible row lengths.
isPowerOf :: Int -> Int -> Bool
isPowerOf x num = num `elem` takeWhile (<= num) [x^n | n <- [0..]]
isPowerOfTwo = isPowerOf 2
isPowerOfFour = isPowerOf 4


-- Helper function to extract sub-ranges of values from a list.
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take to (drop from xs)

-- Given the binary string, return a list-of-lists, where each interior list
-- represents a row of cells to be placed into a constructed pyramid.
parseRows :: String -> [[Cell.Cell]]
parseRows binaryString
    -- If the input length is a power of four, proceed to parse it in
    -- segments with lengths corresponding to increasing odd numbers,
    -- so that the segments represent the rows to be placed.
    | isPowerOfFour strLen  = 
        let lineLengths = takeWhile sufficientSum odds
            offsets = zip (scanl1 (+) ([0] ++ init lineLengths)) lineLengths
            
        -- Reports the Cell values in order from the binary string, in the 
        -- segments needed for each pyramid row.
        in [map Cell.char2cell (slice x y (reverse binaryString)) 
               | (x,y) <- offsets] -- Reverse in the comprehension to simulate 
                                   -- reading from the right. 
    | otherwise = error "Input string length must be a power of 4."
    where strLen = length binaryString
          sufficientSum = \x -> strLen >= sum (takeWhile (<= x) odds)

-- Construct a Pyramid of all zeros for a given number of rows. This will be
-- used to start off the process that recursively 'places' rows when reading
-- from the input string.
makeDummyPyramid :: Int -> Pyramid
makeDummyPyramid 1 = Atomic Cell.Zero
makeDummyPyramid k 
    -- If the prescribed size is a power of two, proceed by making four sub-
    -- pyramids each of half the height.
    | isPowerOfTwo k = (Composite (makeDummyPyramid m) 
                                  (makeDummyPyramid m) 
                                  (makeDummyPyramid m) 
                                  (makeDummyPyramid m))
    | otherwise = error "Pyramid height must be a power of 2."
    where m = div k 2
             
-- Starting from a dummy pyramid of the right height, fold over the list, and
-- to each accumulated pyramid, place the next row of read values.
makePyramid :: String -> Pyramid
makePyramid binaryString = foldl putRow' dummy (zip [1..] rowList)
    where rowList = parseRows binaryString
          dummy = makeDummyPyramid (length rowList)
          putRow' = (\x y -> (putRow x (fst y) (snd y)))
          
-- Bind getRow to the pyramid p, then map that partial function over the 
-- integers corresponing to each row. Concatenate the list of retrieved
-- rows, and print as a right-to-left ordered string.
printPyramid :: Pyramid -> String
printPyramid p = reverse $ 
    map Cell.cell2char (Data.List.concat $ map (getRow p) [1..(getHeight p)])
-----
-----
