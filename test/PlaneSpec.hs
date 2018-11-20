module PlaneSpec where

import Test.Hspec
import Test.HUnit.Lang (Assertion)

import Navlib
import Utilities

planeSpec :: IO ()
planeSpec = hspec $ 
  describe "Plane.hs tests:" $ do
    it "should correctly answer Ch 6 Example 1" $ do
      let c1 = Coord (North $ dms 27 15) (West $ dms 71 23)
      let c2 = Coord (North $ dms 28 11) (West $ dms 68 18)
      planeDist c1 c2 @=? NMiles 173.1
      planeCourse c1 c2 @=? Degrees 71.1
    it "should correctly answer Ch 6 Example 2" $ do
      let c1 = Coord (North $ dms 35 15) (West $ dms 62 23)
      let c2 = Coord (North $ dms 30 25) (West $ dms 70 18)
      planeDist c1 c2 @=? NMiles 493.4
      planeCourse c1 c2 @=? Degrees 234
    it "should correctly answer Ch 6 Example 3" $ do
      let c1 = Coord (South $ dms 40 25.0) (East $ dms 175 50.0)
      let c2 = Coord (South $ dms 35 3.61) (West $ dms 176 5.66)
      let h = Degrees 50
      let d = 500

      let initLat = latToFloat $ lat c1
      initLat @=? (-40.416667)
      let dlat     = (cosine h * d) / 60
      dlat @=? 5.3565
      let finalLat = initLat + dlat
      finalLat @=? (-35.0602)

      let dep      = dlat * tangent h
      dep @=? 6.383667
      let mlat     = initLat + dlat / 2
      mlat @=? (-37.7385)
      let dlon     = dep / cosine (Degrees mlat)
      dlon @=? 8.072333
      let initLon  = lonToFloat $ lon c1
      initLon @=? 175.833333
      let flon     = initLon + dlon
      flon @=? 183.9056666
      let finalLon | flon > 180    = (360 - flon) * (-1)
                   | flon < (-180) = 360 + flon
                   | otherwise     = flon
      finalLon @=? (-176.09433)

      planePosn c1 (Degrees 50) (NMiles 500) @=? c2