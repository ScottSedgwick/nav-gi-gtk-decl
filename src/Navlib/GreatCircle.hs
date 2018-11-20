module Navlib.GreatCircle 
  ( gcInitCourse
  , gcFinalCourse
  , gcDistance
  , courseQuadrant
  , gcVertex
  , Quadrant (..)
  ) where

import Navlib.Types

data Quadrant = NE | NW | SE | SW deriving (Show, Eq)

getFloatCoord :: Coord -> (Float, Float)
getFloatCoord c = (alat, alon)
  where
    alat = latToFloat $ lat c
    alon = lonToFloat $ lon c

getDegCoord :: Coord -> (Degrees Float, Degrees Float)
getDegCoord c = (alat, alon)
  where
    alat = latToDeg $ lat c
    alon = lonToDeg $ lon c

getDegCoords :: Coord -> Coord -> (Degrees Float, Degrees Float, Degrees Float, Degrees Float)
getDegCoords c1 c2 = (latA, lonA, latB, lonB)
  where
    (latA, lonA) = getDegCoord c1
    (latB, lonB) = getDegCoord c2

withDegCoords :: Coord -> Coord -> (Degrees Float -> Degrees Float -> Degrees Float -> Degrees Float -> a) -> a
withDegCoords c1 c2 f = f latA lonA latB lonB
  where
    (latA, lonA) = getDegCoord c1
    (latB, lonB) = getDegCoord c2

courseQuadrant :: Coord -> Coord -> Quadrant
courseQuadrant c1 c2 = withDegCoords c1 c2 f
  where
    f latA lonA latB lonB = q
      where
        east = lonB > lonA || (lonB < lonA) && ((lonA - lonB) > 180)
        north = latB > latA
        q | north     = if east then NE else NW
          | east      = SE 
          | otherwise =  SW

withDabCoords :: Coord -> Coord -> (Degrees Float -> Degrees Float -> Degrees Float -> Degrees Float -> Degrees Float -> Degrees Float -> a) -> a
withDabCoords c1 c2 f = withDegCoords c1 c2 g
  where
    g latA lonA latB lonB = f dab dlon latA lonA latB lonB
      where
        dab  = arccosine $ sine latA * sine latB + cosine latA * cosine latB * cosine dlon :: Degrees Float
        dlon = lonB - lonA

gcInitCourse :: Coord -> Coord -> Heading
gcInitCourse c1 c2 = withDabCoords c1 c2 f
  where 
    f dab dlon latA lonA latB lonB = Heading hdg'
      where
        num  = sine latB - sine latA * cosine dab
        den  = cosine latA * sine dab
        (Degrees hdg)  = arccosine $ num / den
        hdg' = case courseQuadrant c1 c2 of
                NE -> hdg
                NW -> 360 - hdg
                SE -> 360 - hdg
                SW -> 360 - hdg

gcFinalCourse :: Coord -> Coord -> Heading
gcFinalCourse c1 c2 = withDabCoords c1 c2 f
  where 
    f dab dlon latA lonA latB lonB = Heading hdg'
      where
        num = sine latA - sine latB * cosine dab
        den = cosine latB * sine dab
        (Degrees hdg) = arccosine $ num/den
        hdg' = case courseQuadrant c1 c2 of
                NE -> 180 - hdg
                NW -> 180 + hdg
                SE -> 180 + hdg
                SW -> 180 + hdg

gcDistance :: Coord -> Coord -> NMiles
gcDistance c1 c2 = withDabCoords c1 c2 f
  where 
    f dab dlon latA lonA latB lonB = NMiles $ 60 * degToFloat dab

between :: Ord a => a -> a -> a -> Bool
between a b c | a > b     = (c <= a) && (c >= b)
              | otherwise = (c >= a) && (c <= b)

gcVertex :: Coord -> Coord -> Coord
gcVertex c1 c2 = withDegCoords c1 c2 f
  where 
    f latA lonA latB lonB = Coord (degToLat latV) (degToLon lonV)
      where
        (Heading alpha) = gcInitCourse c1 c2
        latV' = arccosine (sine (Degrees alpha) * cosine latA)
        latV'' = if latToFloat (lat c1) < 0 then Degrees (negate (degToFloat latV')) else latV' 
        latV = if between latA latB latV'' then latV'' else Degrees 180 - latV''
        dlon = arccosine (tangent latA / tangent latV)
        lonV' = lonA + dlon
        lonV = if between lonA lonB lonV' then lonV' else lonA + (lonA - lonV')