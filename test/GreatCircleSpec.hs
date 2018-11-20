module GreatCircleSpec where

import Test.Hspec
import Utilities

import Navlib

greatCircleSpec :: IO ()
greatCircleSpec = hspec $ 
  describe "GreatCircle.hs tests:" $ do
    it "should calculate Ch 10 Example 1" $ do
      let c1 = Coord (North $ dms 56 20) (West $ dms 08 12)
      let c2 = Coord (North $ dms 52 12) (West $ dms 57 10)

      courseQuadrant c1 c2 `shouldBe` SW

      let (NMiles d) = gcDistance c1 c2
      1696.5 @=? d

      let (Heading actualInit) = gcInitCourse c1 c2
      282.6 @=? actualInit

      let (Heading actualFinal) = gcFinalCourse c1 c2
      242 @=? actualFinal

    it "should calculate Ch 10 Example 2" $ do
      let c1 = Coord (South $ dms 33 22) (East $ dms 113 08)
      let c2 = Coord (South $ dms 10 51) (East $ dms 049 16)

      courseQuadrant c1 c2 `shouldBe` NW

      let (NMiles d) = gcDistance c1 c2
      3738.1 @=? d

      let (Heading actualInit) = gcInitCourse c1 c2 
      275.2 @=? actualInit

      let (Heading actualFinal) = gcFinalCourse c1 c2
      302.1 @=? actualFinal

    it "should calculate Ch 10 Example 3" $ do
      let c1 = Coord (North $ dms 49 12) (West $ dms 122 50)
      let c2 = Coord (North $ dms 13 30) (East $ dms 145 15)

      courseQuadrant c1 c2 `shouldBe` SE

      let (NMiles d) = gcDistance c1 c2
      4863.4 @=? d

      let (Heading actualInit) = gcInitCourse c1 c2 
      280.3 @=? actualInit

      let (Heading actualFinal) = gcFinalCourse c1 c2
      221.4 @=? actualFinal

    it "should calculate Ch 10 Example 4" $ do
      let c1 = Coord (South $ dms 46 20) (East $ dms 169 10)
      let c2 = Coord (South $ dms 26 25) (West $ dms 105 15)

      courseQuadrant c1 c2 `shouldBe` NE

      let (NMiles d) = gcDistance c1 c2
      4099.1 @=? d

      let (Heading actualInit) = gcInitCourse c1 c2 
      106.1 @=? actualInit

      -- let (Heading actualFinal) = gcFinalCourse c1 c2
      -- 242 @=? actualFinal

    it "should calculate Ch 10 Example 5" $ do
      let c1 = Coord (South $ dms 17 0) (East $ dms 170 0)
      let c2 = Coord (North $ dms 22 0) (West $ dms 110 0)

      courseQuadrant c1 c2 `shouldBe` NE

      let (NMiles d) = gcDistance c1 c2
      5247.2 @=? d

      let (Heading actualInit) = gcInitCourse c1 c2 
      66.1 @=? actualInit

      let (Heading actualFinal) = gcFinalCourse c1 c2
      70.5 @=? actualFinal


    it "should calculate Ch 10 Example 6" $ do
      let c1 = Coord (South $ dms 45 44) (East $ dms 171 15)
      let c2 = Coord (North $ dms 07 30) (West $ dms 079 21)

      -- let (NMiles d1) = planeDist c1 c2
      -- 6159.1 @=? d1

      let (NMiles d2) = gcDistance c1 c2
      6531.9 @=? d2

    -- it "should calculate Ch 10 Example 7" $ do
    --   let c1 = Coord (South $ dms 34 55) (West $ dms 56 10)
    --   let c2 = Coord (South $ dms 33 55) (East $ dms 18 25)

    --   let (Heading alpha) = gcInitCourse c1 c2
    --   alpha @=? 112.5
    --   let latA = latToDeg $ lat c1
    --   let latV' = arccosine (sine (Degrees alpha) * cosine latA)
    --   let latV = if latToFloat (lat c1) < 0 then Degrees (negate (degToFloat latV')) else latV' 
    --   latV @=? Degrees (-40.746667)
    --   let (Degrees dlon) = arccosine (tangent latA / tangent latV)
    --   dlon @=? (35.883333)
    --   let lonA = lonToFloat $ lon c1
    --   lonA @=? (-56.166666)
    --   let lonV = Degrees $ lonA + dlon
    --   lonV @=? Degrees (-20.283333)

    --   let expected = Coord (South $ dms 40 44.8) (West $ dms 20 17.0)
    --   let actual = gcVertex c1 c2
    --   expected @=? actual

    -- it "should calculate Ch 10 Example 8" $ do
    --   let c1 = Coord (North $ dms 51 25) (West $ dms 09 30)
    --   let c2 = Coord (North $ dms 46 00) (West $ dms 49 00)
    --   let ev = Coord (North $ dms 51 31.1) (West $ dms 14 24)
    --   let dist = gcDistance c1 c2
    --   dist @=? (NMiles 1577.1)
    --   let initCourse = gcInitCourse c1 c2
    --   initCourse @=? (Heading 273.8)
    --   let finalCourse = gcFinalCourse c1 c2
    --   finalCourse @=? (Heading 243.6)
    --   let vertex = gcVertex c1 c2
    --   vertex @=? ev
  