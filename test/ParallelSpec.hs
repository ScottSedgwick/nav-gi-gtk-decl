module ParallelSpec where

import Test.Hspec
import Test.HUnit.Lang (Assertion)

import Navlib
import Utilities

parallelSpec :: IO ()
parallelSpec = hspec $ 
  describe "Parallel.hs tests:" $ do
    it "should correctly answer Ch 5 Example 1" $ 
      departure (North (dms 35 20)) (West (dms 15 31)) (West (dms 25 50)) @=? NMiles 505
    it "should correctly answer Ch 5 Example 2" $ 
      departure (South (dms 30 0)) (West (dms 171 0)) (East (dms 178 0)) @=? NMiles 571.6