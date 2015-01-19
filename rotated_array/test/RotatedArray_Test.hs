module RotatedArray_Test where
import Test.HUnit
import RotatedArray

{---
 | Test creation of RotatedArray from list.  
 ---}
fromList_Cases = TestLabel "Tests for fromList" (
    TestList [testEmpty, testSorted])

testEmpty = TestCase $ assertEqual 
  "Should get RotatedArray [] from empty list" 
  (RotatedArray [])
  (fromList 0 [])

testSorted = TestCase $ assertEqual 
  "Should get sorted RotatedArray from unsorted input"
  (RotatedArray [1, 2, 3, 4])
  (fromList 0 [4, 2, 3, 1])

{---
 | Test rotating a RotatedArray.
 ---}
rotateBy_Cases = TestLabel "Tests for rotateBy" (
    TestList [testRotateEmpty, testRotateZero, testRotateNegative])

testRotateEmpty = TestCase $ assertEqual 
  "Rotation should not change an Empty RotationArray" 
  (RotatedArray [])
  (rotateBy 20 (RotatedArray []))
 
testRotateZero = TestCase $ assertEqual 
  "Rotation by zero should not change anything" 
  (fromList 11 [1,2,3,4])
  (rotateBy 0 (fromList 11 [1,2,3,4]))

testRotateNegative = TestCase $ assertEqual 
  "Rotation by negative should work the same way" 
  (fromList 11 [1,2,3,4])
  (rotateBy 0 (fromList 11 [1,2,3,4]))

{---
 | Test computing the rotation of a RotatedArray.
 ---}
getRotation_Cases = TestLabel "Tests for getRotation" (
    TestList [testGetEmpty, testGetSorted, testGetUnsorted])

testGetEmpty = TestCase $ assertEqual
  "Rotation of empty RotatedArray should be 0" 
  (getRotation (fromList 0 []))
  (0)

testGetSorted = TestCase $ assertEqual 
  "Rotation of already sorted array should be 0" 
  (getRotation (RotatedArray [1, 2, 3, 5]))
  (0)

testGetUnsorted = TestCase $ assertEqual 
  "Rotation of test examples should be [1, 2]" 
  (map getRotation [RotatedArray [5, 1, 2, 3, 4], 
                    RotatedArray [4, 5, 1, 2, 3]])
  ([1, 2])

 
{---
 | Hook for running test suite.
 ---}
main = runTestTT $ TestList [fromList_Cases, 
                             rotateBy_Cases, 
                             getRotation_Cases]
