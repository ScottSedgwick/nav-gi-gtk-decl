module Navlib.Plane
  ( planeCourse
  , planeDist
  , planePosn
  ) where

import Navlib.Types
import Navlib.Parallel

dlat :: Latitude -> Latitude -> Degrees Float
dlat lat1 lat2 = Degrees $ abs $ latToFloat lat1 - latToFloat lat2

minutes :: Degrees Float -> Float
minutes (Degrees f) = f * 60

pc :: Coord -> Coord -> Degrees Float
pc (Coord lat1 lon1) (Coord lat2 lon2) = arctangent $ dep / minutes dlt
  where
    (NMiles dep)  = departure avgLat lon1 lon2
    avgLat  = if avgLatF < 0 then South (Degrees $ abs avgLatF) else North (Degrees avgLatF)
    avgLatF = (latToFloat lat1 + latToFloat lat2) / 2
    dlt = dlat lat1 lat2

westOf :: Longtitude -> Longtitude -> Bool
westOf (East (Degrees l1)) (East (Degrees l2)) = l2 < l1
westOf (East (Degrees l1)) (West (Degrees l2)) = l1 + l2 > 180
westOf (West (Degrees l1)) (West (Degrees l2)) = l1 > l2
westOf (West (Degrees l1)) (East (Degrees l2)) = l1 + l2 < 180

planeCourse :: Coord -> Coord -> Degrees Float
planeCourse c1 c2 = c'
  where
    c = pc c1 c2
    c' = if westOf (lon c2) (lon c1) then 180 + c else c

planeDist :: Coord -> Coord -> NMiles
planeDist c1 c2 = NMiles $ minutes dlt / cosine c
  where
    dlt = dlat (lat c1) (lat c2)
    c = pc c1 c2

planePosn :: Coord -> Degrees Float -> NMiles -> Coord
planePosn c h (NMiles d) = Coord { lat = floatToLat finalLat, lon = floatToLon finalLon }
  where
    initLat = latToFloat $ lat c
    dlat     = (cosine h * d) / 60
    finalLat = initLat + dlat
    dep      = dlat * tangent h
    mlat     = initLat + dlat / 2
    dlon     = dep / cosine (Degrees mlat)
    initLon  = lonToFloat $ lon c
    flon     = initLon + dlon
    finalLon | flon > 180    = (360 - flon) * (-1)
             | flon < (-180) = 360 + flon
             | otherwise     = flon