module CartesianSpec where

import Test.Hspec
import Utilities
import Control.Monad (forM_)

import Navlib

type ToFromCartesianTestItem = ((Float, Float), (Float, Float, Float))

toFromCartesianTests :: [ToFromCartesianTestItem]
toFromCartesianTests = 
  [ (( 000,  000), ( 1.000,  0.000,  0.000))
  , (( 045,  000), ( 0.707,  0.707,  0.000))
  , (( 090,  090), ( 0.000,  1.000,  0.000))
  , ((-045,  000), ( 0.707, -0.707,  0.000))
  , ((-090,  090), ( 0.000, -1.000,  0.000))
  
  , ((-045,  090), ( 0.000, -0.707,  0.707))
  , (( 000,  045), ( 0.707,  0.000,  0.707))
  , (( 000,  135), (-0.707,  0.000,  0.707))
  , (( 000, -135), (-0.707,  0.000, -0.707))
  , (( 000, -045), ( 0.707,  0.000, -0.707))
  ]

cartesianSpec :: IO ()
cartesianSpec = hspec $ 
  describe "CartesianSpec.hs tests:" $ do
    forM_ toFromCartesianTests $ \((lat, lon), (ax, ay, az)) -> 
      it ("should convert from lat/lon (" ++ show lat ++ ", " ++ show lon ++ ") to xyz (" ++ show ax ++ ", " ++ show ay ++ ", " ++ show az ++ ") correctly") $ do
        let (Cartesian x y z) = polarToCartesian (floatToLat lat) (floatToLon lon)
        x @=? ax
        y @=? ay
        z @=? az
    forM_ toFromCartesianTests $ \((lat, lon), (ax, ay, az)) -> 
      it ("should convert from xyz (" ++ show ax ++ ", " ++ show ay ++ ", " ++ show az ++ ") to lat/lon (" ++ show lat ++ ", " ++ show lon ++ ") correctly") $ do
        let (latr, lonr) = cartesianToPolar (Cartesian ax ay az)
        latr @=? floatToLat lat
        lonr @=? floatToLon lon