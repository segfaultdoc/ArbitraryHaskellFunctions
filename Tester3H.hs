{-

load this file and call main. Alternatively, run this command to run
just the expression "main", from the terminal:

    ghc -e "main" Tester3H.hs

All errors and failures count against your score out of 100 points.

You can also use the testFunc function to only test a specific
function (look through the file for the exact name, but it's always
"test_" and the official name from the spec). Example:

    testFunc test_trib

Be sure to :reload between calls!

-}

import Homework3
import Test.HUnit
import Control.Exception
import Control.Monad


import Prelude hiding (zipWith)
import Data.Char(ord)

tc s a b = TestCase $ assertEqual s a b

main = runTestTT $ TestList
       [
         test_primeFactors,
         test_coprime,
         test_trib,
         test_maxNew,
         test_zipWith,
         test_passFail,
         test_powerset,
         test_matrixProduct
       ]

testFunc tl = runTestTT tl


test_primeFactors = TestList [
  tc "primeFactors 01" [] $ primeFactors 1,
  tc "primeFactors 02" [2] $ primeFactors 2,
  tc "primeFactors 03" [3] $ primeFactors 3,
  tc "primeFactors 04" [2,2] $ primeFactors 4,
  tc "primeFactors 05" [5] $ primeFactors 5,
  tc "primeFactors 06" [2,5,5] $ primeFactors 50,
  tc "primeFactors 07" [2,3,11] $ primeFactors 66,
  tc "primeFactors 08" [2,2,5,5] $ primeFactors 100,
  tc "primeFactors 09" [463] $ primeFactors 463,
  tc "primeFactors 10" [2,2,2,2,2,2,2,2,2] $ primeFactors 512,
  tc "primeFactors 11" [3,3,13] $ primeFactors 117,
  tc "primeFactors 12" [1117] $ primeFactors 1117,
  tc "primeFactors 13" [3,3,3607,3803] $ primeFactors 123456789
  ]

test_coprime = TestList [
 tc "coprime 01" True  $ coprime 2 3,
 tc "coprime 02" True  $ coprime 3 7,
 tc "coprime 03" True  $ coprime 3 10,
 tc "coprime 04" False $ coprime 10 15,
 tc "coprime 05" False $ coprime 15 10,
 tc "coprime 06" True  $ coprime 50 61,
 tc "coprime 07" False $ coprime 100 200,
 tc "coprime 08" True  $ coprime 97 98,
 tc "coprime 09" False $ coprime 66 201,
 tc "coprime 10" False $ coprime 49 28,
 tc "coprime 11" True  $ coprime 367 463,
 tc "coprime 12" True  $ coprime 330 463,
 tc "coprime 13" True  $ coprime 441 1000
 ]

test_trib = TestList [
  tc "trib 01"        1 $ trib  0,
  tc "trib 02"        1 $ trib  2,
  tc "trib 03"        3 $ trib  3,
  tc "trib 04"        5 $ trib  4,
  tc "trib 05"        9 $ trib  5,
  tc "trib 06"       17 $ trib  6,
  tc "trib 07"       31 $ trib  7,
  tc "trib 08"      193 $ trib 10,
  tc "trib 09"      653 $ trib 12,
  tc "trib 10"     1201 $ trib 13,
  tc "trib 11"     2209 $ trib 14,
  tc "trib 12"    85525 $ trib 20,
  tc "trib 13" 37895489 $ trib 30
 ]

test_maxNew = TestList [
  tc "maxNew 01" Nothing     $ maxNew [] [],
  tc "maxNew 02" (Just (5::Int)) $ maxNew [5::Int] [],
  tc "maxNew 03" Nothing     $ maxNew [] [5::Int],
  tc "maxNew 04" (Just 4)    $ maxNew [1,2,3,4,5,6] [5,6],
  tc "maxNew 05" (Just 4)    $ maxNew [1,5,3,4,6,2,4,4] [5,6],
  tc "maxNew 06" (Just (-3)) $ maxNew [-5,-4,-3] [-4],
  tc "maxNew 07" (Just (-3)) $ maxNew [-3,3] [3],
  tc "maxNew 08" Nothing     $ maxNew [1,1,1,1,1] [1],
  tc "maxNew 09" (Just 20)   $ maxNew [20,10] [200],
  tc "maxNew 10" (Just 3)    $ maxNew [1,2,3] [4,5,6],
  tc "maxNew 11" (Just 463)  $ maxNew [112,211,262,330,367,463] [330,367],
  tc "maxNew 12" (Just 262)  $ maxNew [112,211,262,330,367,463] [330,367,463]
 ]


test_zipWith = TestList [
  tc "zipWith 01" [11,12,13,14]$ zipWith (+) [1,2,3,4] [10,10,10,10],
  tc "zipWith 02" [11,12,13,14]$ zipWith (+) [1,2,3,4] [10,10,10,10],
  tc "zipWith 03" [6,8,10,12]$ zipWith (+) [1,2,3,4] [5,6,7,8],
  tc "zipWith 04" [6,8,10,12]$ zipWith (+) [1,2,3,4] [5,6,7,8],
  tc "zipWith 05" [10,15,20] $ zipWith (*) [2,3,4] [5,5,5,5],
  tc "zipWith 06" [10,15,20] $ zipWith (*) [2,3,4] [5,5,5,5],
  tc "zipWith 07" [10,15,20] $ zipWith (*) [2,3,4,5,6,7,8] [5,5,5],
  tc "zipWith 08" [10,15,20] $ zipWith (*) [2,3,4,5,6,7,8] [5,5,5],
  tc "zipWith 09" [] $ zipWith (*) [1,2,3,4,5] ([]::[Int]),
  tc "zipWith 10" [] $ zipWith (*) [1,2,3,4,5] ([]::[Int]),
  tc "zipWith 11" [] $ zipWith (*) ([]::[Int]) [1,2,3,4,5],
  tc "zipWith 12" [] $ zipWith (*) ([]::[Int]) [1,2,3,4,5],
  tc "zipWith 13" [] $ zipWith (*) ([]::[Int]) ([]::[Int])
  ]

test_passFail = TestList [
  tc "passFail 01" ([2,4],[1,3,5])      $ passFail even [1,2,3,4,5],
  tc "passFail 02" ([],[9,9,9])         $ passFail even [9,9,9],
  tc "passFail 03" ([6,8,10],[])        $ passFail even [6,8,10],
  
  tc "passFail 04" ([1,3,2],[-1,-3,-2]) $ passFail pos [1,-1,3,-3,2,-2],
  tc "passFail 05" ([],[-3,-4,-5])      $ passFail pos [-3,-4,-5],
  
  tc "passFail 06" ([11,12,13],[8,9,10,1]) $ passFail big [8,9,10,11,12,13,1],
  tc "passFail 07" ([],[10,10,10])      $ passFail big [10,10,10],
  
  tc "passFail 08" ("H","ello")    $ passFail cap "Hello",
  tc "passFail 09" ("CS"," 463")   $ passFail cap "CS 463",
  tc "passFail 10" ("RNO","asm!")  $ passFail cap "RaNsOm!",
  tc "passFail 11" ([[1..15],[1..100]],[[1,2],[1,2,3]]) $ passFail lengthy [[1,2],[1,2,3],[1..15],[1..100]],
  tc "passFail 12"  (["hello","you?"],["hi","how","are"]) $ passFail lengthy ["hi","hello","how","are","you?"]
 ]
    where big x = x>10
          pos x = x>0
          cap x = (64 <= ord x) && (ord x <= 90)
          lengthy xs = 3<length xs

test_powerset = TestList [
  tc "powerset 01" True $ sameItems (powerset []) ([[]]::[[Int]]),
  tc "powerset 02" True $ sameItems (powerset [1]) [[],[1]],
  tc "powerset 03" True $ sameItems (powerset [5]) [[],[5]],
  tc "powerset 04" True $ sameItems (powerset [1,2]) [[],[1],[2],[1,2]],
  tc "powerset 05" True $ sameItems (powerset [2,1]) [[],[2],[1],[2,1]],
  tc "powerset 06" True $ sameItems (powerset [1,2,3]) [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]],
  tc "powerset 07" True $ sameItems (powerset [3,1,2]) [[],[3],[1],[2],[3,1],[3,2],[1,2],[3,1,2]],
  tc "powerset 08" True $ sameItems (powerset [-4,-5]) [[],[-4],[-5],[-4,-5]],
  tc "powerset 09" True $ sameItems (powerset [1,2,3,4]) [[],[1],[2],[3],[4],[1,2],[1,3],[2,3],[1,4],[2,4],[3,4],[1,2,3],[1,2,4],[1,3,4],[2,3,4],[1,2,3,4]],
  tc "powerset 10" True $ sameItems (powerset [0,2,4,6,8]) [[],[0],[2],[4],[6],[8],[0,2],[0,4],[2,4],[0,6],[2,6],[4,6],[0,8],[2,8],[4,8],[6,8],[0,2,4],[0,2,6],[0,4,6],[2,4,6],[0,2,8],[0,4,8],[2,4,8],[0,6,8],[2,6,8],[4,6,8],[0,2,4,6],[0,2,4,8],[0,2,6,8],[0,4,6,8],[2,4,6,8],[0,2,4,6,8]],
  tc "powerset 11" True $ sameItems (powerset [4,3,2,1]) [[],[4],[3],[2],[1],[4,3],[4,2],[3,2],[4,1],[3,1],[2,1],[4,3,2],[4,3,1],[4,2,1],[3,2,1],[4,3,2,1]],
  tc "powerset 12" True $ sameItems (powerset [9,8,7,6,5]) [[],[9],[8],[7],[6],[5],[9,8],[9,7],[8,7],[9,6],[8,6],[7,6],[9,5],[8,5],[7,5],[6,5],[9,8,7],[9,8,6],[9,7,6],[8,7,6],[9,8,5],[9,7,5],[8,7,5],[9,6,5],[8,6,5],[7,6,5],[9,8,7,6],[9,8,7,5],[9,8,6,5],[9,7,6,5],[8,7,6,5],[9,8,7,6,5]]
 ]
    where 
      sameItems xss yss = subSet xss yss && subSet yss xss
      subSet xss yss = and $ (map (\xs-> or $ map (sameNums xs) yss) xss)
      sameNums xs ys = and $ map (\x->x `elem` ys) xs


test_matrixProduct = TestList [
    tc "matrixProduct 01" [[32]] $ matrixProduct [[1,2,3]] [[4],[5],[6]],
    tc "matrixProduct 02" [[4, 8, 12], [5, 10, 15], [6, 12, 18]] $ matrixProduct [[4],[5],[6]] [[1,2,3]],
    tc "matrixProduct 03" [[19, 22], [43, 50]] $ matrixProduct [[1,2],[3,4]] [[5,6],[7,8]],
    tc "matrixProduct 04" [[5, 6], [7, 8]] $ matrixProduct [[1,0],[0,1]] [[5,6],[7,8]],
    tc "matrixProduct 05" [[5, 6], [7, 8]] $ matrixProduct [[5, 6], [7, 8]] [[1,0],[0,1]],
    tc "matrixProduct 06" [[6]] $ matrixProduct [[1,2,3]] [[1],[1],[1]],
    tc "matrixProduct 07" [[12]] $ matrixProduct [[1,2,3]] [[2],[2],[2]],
    tc "matrixProduct 08" [[1, 1, 1], [2, 2, 2], [3, 3, 3]] $ matrixProduct [[1],[2],[3]] [[1,1,1]],
    tc "matrixProduct 09" [[0, 0, 0], [0, 0, 0], [0, 0, 0]] $ matrixProduct [[1],[2],[3]] [[0,0,0]],
    tc "matrixProduct 10" [[1, 2, 0], [3, 4, 0]] $ matrixProduct [[1,2],[3,4]] [[1,0,0],[0,1,0]],
    tc "matrixProduct 11" [[1, 2, 0], [3, 4, 0], [5, 6, 0]] $ matrixProduct [[1,2],[3,4],[5,6]] [[1,0,0],[0,1,0]],
    tc "matrixProduct 12" [[11*13]] $ matrixProduct [[11]] [[13]]
    
 ]
