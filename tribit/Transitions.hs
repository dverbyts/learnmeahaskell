module Transitions where
import qualified Cell
import qualified Pyramid
import qualified Data.Map
import qualified Data.List
import qualified Data.Maybe

---------------------------------
-- Map containing transitions. --
---------------------------------
transitions = Data.Map.fromList [
    ("0000", "0000"),
    ("0001", "1000"),
    ("0010", "0001"),
    ("0011", "0010"),
    ("0100", "0000"),
    ("0101", "0010"),
    ("0110", "1011"),
    ("0111", "1011"),
    ("1000", "0100"),
    ("1001", "0101"),
    ("1010", "0111"),
    ("1011", "1111"),
    ("1100", "1101"),
    ("1101", "1110"),
    ("1110", "0111"),
    ("1111", "1111")
    ]
-----
-----

------------------------------          
-- Pyramid reduction rules. --          
------------------------------
constCollapse :: Pyramid.Pyramid -> Maybe Pyramid.Pyramid
-- Return an atomic constant pyramid from a composite constant pyramid
constCollapse p = if (Data.Maybe.isJust x)
    then Just (Pyramid.Atomic (Data.Maybe.fromJust x))    
    else Nothing      
    where x = Pyramid.getConst p 
  
          
patternCollapse :: Pyramid.Pyramid -> Maybe Pyramid.Pyramid
-- Pattern-based size reduction doesn't apply to a sub-pyramid with only cells.
patternCollapse (Pyramid.Composite (Pyramid.Atomic _) 
                                   (Pyramid.Atomic _) 
                                   (Pyramid.Atomic _) 
                                   (Pyramid.Atomic _)) = Nothing
                                                         
-- When all sub-pyramids are each constant-valued, directly construct the 
-- reduced-size pyramid. Otherwise, recursively checks if it applies one
-- level down, and if not returns Nothing.
patternCollapse (Pyramid.Composite p1 p2 p3 p4)
    -- Reduce when all subpyramids have constant Cell values.
    | Data.List.all (Data.Maybe.isJust) consts  = 
          Just (Pyramid.Composite (Pyramid.Atomic (Data.Maybe.fromJust c1)) 
                                  (Pyramid.Atomic (Data.Maybe.fromJust c2)) 
                                  (Pyramid.Atomic (Data.Maybe.fromJust c3)) 
                                  (Pyramid.Atomic (Data.Maybe.fromJust c4)))
          
    -- Recursive check going down one level of pyramids.
    | otherwise = 
          let subPatterns@(sp1:sp2:sp3:sp4:_) = 
                  map patternCollapse [p1, p2, p3, p4]
          in if Data.List.all (Data.Maybe.isJust) subPatterns    
              then Just (Pyramid.Composite (Data.Maybe.fromJust sp1)
                                           (Data.Maybe.fromJust sp2)
                                           (Data.Maybe.fromJust sp3)
                                           (Data.Maybe.fromJust sp4))
              else Nothing     
    where consts@(c1:c2:c3:c4:_) = map Pyramid.getConst [p1, p2, p3, p4]
-----
-----
          
------------------------------------------------------------
-- Recursively define application of pyramid transitions. --
------------------------------------------------------------
outerTransition :: Pyramid.Pyramid -> Pyramid.Pyramid
-- Check at the whole pyramid level for single-step constant reduction or
-- group-level pattern reduction. Pass off to recursive inner transition 
-- for other transition steps.
outerTransition composite = 
    let x = constCollapse composite
    in if (Data.Maybe.isJust x)
       then Data.Maybe.fromJust x   
       else let y = patternCollapse composite     
            in if (Data.Maybe.isJust y)    
               then Data.Maybe.fromJust y   
               else innerTransition composite     

innerTransition :: Pyramid.Pyramid -> Pyramid.Pyramid
-- Recursively check for matches to the transition rules and construct new
-- pyramid embodying the transition. This is invoked inside of the outer
-- transition which already takes care of global or group shortcut steps.
innerTransition (Pyramid.Atomic c) = 
    error "Inner transition should not be applied to a single Cell."
    
-- When applied to a basic pyramid with only cells, read the cells in the
-- specified order as binary characters, match them to the transition and
-- construct a new pyramid with the appropriate new cells.
innerTransition (Pyramid.Composite (Pyramid.Atomic c1) 
                                   (Pyramid.Atomic c2) 
                                   (Pyramid.Atomic c3) 
                                   (Pyramid.Atomic c4)) = 
    (Pyramid.Composite (Pyramid.Atomic nc1) 
                       (Pyramid.Atomic nc2) 
                       (Pyramid.Atomic nc3) 
                       (Pyramid.Atomic nc4))
    -- Reverse to reflect reading order of Pyramid cells vs. right-to-left 
    -- binary string.
    where oldBinary           = reverse $ map Cell.cell2char [c1, c2, c3, c4]
          newMaybe            = Data.Map.lookup oldBinary transitions
          newBinary           = Data.Maybe.fromJust newMaybe
          (nc1:nc2:nc3:nc4:_) = reverse $ map Cell.char2cell newBinary

-- When applied to a composite pyramid, pass recursively and construct a new
-- pyramid with the respective transition results.
innerTransition (Pyramid.Composite p1 p2 p3 p4) = 
    (Pyramid.Composite (innerTransition p1) 
                       (innerTransition p2)
                       (innerTransition p3)
                       (innerTransition p4))
-----
-----
    
------------------------------------------------
-- Helpers for IO application of transitions. --
------------------------------------------------
reportStep :: Pyramid.Pyramid -> (String, Pyramid.Pyramid)
-- Perform one recursive transition and report the string and new pyramid that
-- results.
reportStep oldPyramid = 
    let newPyramid = outerTransition oldPyramid
        newString = Pyramid.printPyramid newPyramid
    in (newString, newPyramid)

returnSteps :: Pyramid.Pyramid -> [String] -> [String]
-- Recursively concatenates the output of reportStep to build up a list of the
-- successive pyramids in the chain of transitions. This list can then be
-- passed to any printing or IO representation and it helps keep all of the IO
-- sequestered into Main.

returnSteps (Pyramid.Atomic c) outputList
  | outputList == [] = [[Cell.cell2char c]] -- inner list to go char -> string
  | otherwise        = outputList 
returnSteps pyramid outputList = 
    let (newStr, newPyr) = reportStep pyramid 
    in returnSteps newPyr $ outputList ++ [newStr]
-----    
-----

