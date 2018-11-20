module HorizonSpec (horizonSpec) where

import Test.Hspec
import Test.HUnit.Approx (assertApproxEqual)

import Navlib

shouldBeNear = assertApproxEqual "" (NMiles 0.01)

horizonSpec :: IO ()
horizonSpec = hspec $ 
  describe "Horizon.hs tests:" $ do
    it "should calculate Ch 3 Example 1" $ 
      visibleHorizon (Metres 15) `shouldBeNear` NMiles 8.02 
    it "should calculate Ch 3 Example 2" $ 
      radarHorizon (Metres 23) `shouldBeNear` NMiles 10.6