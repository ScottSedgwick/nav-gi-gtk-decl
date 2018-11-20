module DirectionSpec (directionSpec) where

import Test.Hspec

import Navlib

directionSpec :: IO ()
directionSpec = hspec $ 
  describe "Direction.hs tests:" $ do
    it "trueToMagnetic should convert true to magnetic bearings" $ do
      trueToMagnetic (Variation $ BWest 10) (TrueHeading 0) `shouldBe` MagneticHeading  10 
      trueToMagnetic (Variation $ BEast  0) (TrueHeading 0) `shouldBe` MagneticHeading   0 
      trueToMagnetic (Variation $ BEast 10) (TrueHeading 0) `shouldBe` MagneticHeading 350 
    it "compassError should calculate compass error" $ do
      compassError (Variation $ BEast 25) (Deviation $ BWest 10) `shouldBe` CompassError (BEast 15)
      compassError (Variation $ BWest 21) (Deviation $ BEast 10) `shouldBe` CompassError (BWest 11)
      compassError (Variation $ BEast 15) (Deviation $ BEast  4) `shouldBe` CompassError (BEast 19)
      compassError (Variation $ BWest 11) (Deviation $ BWest  9) `shouldBe` CompassError (BWest 20)
    it "should correctly calculate Ch 1 Example 2" $ do
      compassToTrue (Variation $ BWest 25) (Deviation $ BEast 12) (CompassHeading 130) `shouldBe` TrueHeading 117
      compassToMagnetic (Deviation $ BEast 12) (CompassHeading 130) `shouldBe` MagneticHeading 142
      magneticToTrue (Variation $ BWest 25) (MagneticHeading 142) `shouldBe` TrueHeading 117
      trueToMagnetic (Variation $ BWest 25) (TrueHeading 117) `shouldBe` MagneticHeading 142
      magneticToCompass (Deviation $ BEast 12) (MagneticHeading 142) `shouldBe` CompassHeading 130
      trueToCompass (Variation $ BWest 25) (Deviation $ BEast 12) (TrueHeading 117) `shouldBe` CompassHeading 130
    it "should correctly calculate Ch 1 Example 3" $ do
      trueToCompass (Variation $ BEast 9) (Deviation $ BWest 27) (TrueHeading 250) `shouldBe` CompassHeading 268
      compassToTrue (Variation $ BEast 9) (Deviation $ BWest 27) (CompassHeading 268) `shouldBe` TrueHeading 250
    it "should correctly calculate Ch 1 Example 4" $ do
      findCompassError (TrueHeading 130) (CompassHeading 105) `shouldBe` CompassError (BEast 25)
      findDeviation (CompassError (BEast 25)) (Variation (BEast 12)) `shouldBe` Deviation (BEast 13)
      findVariation (CompassError (BEast 25)) (Deviation (BEast 13)) `shouldBe` Variation (BEast 12)