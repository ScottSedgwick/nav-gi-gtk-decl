module Navlib.Cartesian where

import Navlib.Types

data Cartesian = Cartesian { x :: Float, y :: Float, z :: Float } deriving (Eq, Show)

polarToCartesian :: Latitude -> Longtitude -> Cartesian
polarToCartesian lat lon = Cartesian { x = ax, y = ay, z = az }
  where
    lat' = case lat of
      North x -> x
      South x -> negate x
    lon' = case lon of
      East x -> x
      West x -> negate x
    ay = sine lat'
    dy = cosine lat'
    ax = dy * cosine lon'
    az = dy * sine lon'

cartesianToPolar :: Cartesian -> (Latitude, Longtitude)
cartesianToPolar (Cartesian x y z) = (degToLat lat, degToLon lon)
  where
    lat = arcsine y
    ln1 = if x == 0                      -- Deal with the invalid TAN case
          then if z >= 0 
               then Degrees 90 
               else Degrees 270
          else arctangent $ z / x
    lon = if x >= 0
          then if z >= 0
               then ln1                  -- 0..90
               else Degrees 360 + ln1    -- 270..360
          else Degrees 180 + ln1