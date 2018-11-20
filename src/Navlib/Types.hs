{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Navlib.Types
  ( Height(..)
  , NMiles(..)
  , Angle(..)
  , Degrees(..)
  , Heading (..)
  , Bearing (..)
  , CompassHeading (..)
  , MagneticHeading (..)
  , TrueHeading (..)
  , CompassError (..)
  , Deviation (..)
  , Variation(..)
  , Latitude(..)
  , Longtitude(..)
  , Coord(..)
  , bearingToHeading
  , dms
  , degToFloat
  , latToDeg
  , latToFloat
  , lonToDeg
  , lonToFloat
  , floatToLon
  , floatToLat
  , degToLon
  , degToLat
  ) 
where

import Data.Fixed (divMod')

data Height = Feet Float
            | Metres Float
            deriving (Show, Eq)

newtype NMiles = NMiles Float deriving (Show, Eq, Ord)
instance Num NMiles where
  (+) (NMiles a) (NMiles b) = NMiles $ a + b
  (-) (NMiles a) (NMiles b) = NMiles $ a - b
  (*) (NMiles a) (NMiles b) = NMiles $ a * b
  abs (NMiles a) = NMiles $ abs a
  fromInteger = NMiles . fromInteger
  negate (NMiles a) = NMiles $ a * (-1)
  signum (NMiles a) = NMiles $ signum a

degreesInCircle :: Float
degreesInCircle = 360

fmod :: Float -> Float -> Float
fmod v m | v > m = (v - m) `fmod` m
          | v < 0 = (v + m) `fmod` m
          | otherwise = v

newtype Heading = Heading Float deriving (Show, Eq, Ord)
instance Num Heading where
  (+) (Heading a) (Heading b) = abs $ Heading (a + b)
  (-) (Heading a) (Heading b) = abs $ Heading (a - b)
  (*) (Heading a) (Heading b) = abs $ Heading (a * b)
  abs (Heading a) = Heading $ a `fmod` degreesInCircle
  fromInteger i = abs $ Heading $ fromIntegral i
  negate (Heading a) = abs $ Heading (a * (-1))
  signum (Heading a) | a < 0 = Heading (-1)
                      | a > 0 = Heading 1
                      | otherwise = Heading 0
-- instance Ord Heading where
--   compare (Heading a) (Heading b) = compare a b


data Bearing = BEast Float | BWest Float deriving (Show, Eq)
instance Num Bearing where
  (+) a b = floatToBearing $ bearingToFloat a + bearingToFloat b
  (-) a b = floatToBearing $ bearingToFloat a - bearingToFloat b
  (*) a b = floatToBearing $ bearingToFloat a * bearingToFloat b
  abs b = if z < 0 then BWest (z * (-1)) else BEast z
    where
      z = if y > (degreesInCircle / 2) then y - degreesInCircle else y
      y = x `fmod` degreesInCircle
      x = case b of
            BEast e -> e
            BWest w -> w * (-1)
  fromInteger i = floatToBearing $ fromIntegral i
  negate a = abs $ a * BWest 1
  signum (BEast a) = if a == 0 then 0 else 1
  signum (BWest _) = -1

newtype CompassHeading = CompassHeading Heading deriving (Show, Eq)

newtype MagneticHeading = MagneticHeading Heading deriving (Show, Eq)

newtype TrueHeading = TrueHeading Heading deriving (Show, Eq)

newtype CompassError = CompassError Bearing deriving (Show, Eq)

newtype Deviation = Deviation Bearing deriving (Show, Eq)

newtype Variation = Variation Bearing deriving (Show, Eq)

bearingToFloat :: Bearing -> Float
bearingToFloat (BEast f) = f
bearingToFloat (BWest f) = f * (-1)

bearingToHeading :: Bearing -> Heading
bearingToHeading = abs . Heading . bearingToFloat

floatToBearing :: Float -> Bearing
floatToBearing a = if c < 0 then BWest (c * (-1)) else BEast c
  where
    c = if b > (degreesInCircle / 2) then b - degreesInCircle else b
    b = a `fmod` degreesInCircle

-- | An angle in radians.
newtype Radians x = Radians x deriving (Eq, Ord, Show, Num, Fractional)

-- | An angle in degrees.
newtype Degrees x = Degrees x deriving (Eq, Ord, Show, Num, Fractional)

-- | Convert from radians to degrees.
degrees :: (Floating x) => Radians x -> Degrees x
degrees (Radians x) = Degrees (x/pi*180)

-- | Convert from degrees to radians.
radians :: (Floating x) => Degrees x -> Radians x
radians (Degrees x) = Radians (x/180*pi)

-- | Type-class for angles.
class Angle a where
  sine    :: (Floating x) => a x -> x
  cosine  :: (Floating x) => a x -> x
  tangent :: (Floating x) => a x -> x

  arcsine    :: (Floating x) => x -> a x
  arccosine  :: (Floating x) => x -> a x
  arctangent :: (Floating x) => x -> a x

instance Angle Radians where
  sine    (Radians x) = sin x
  cosine  (Radians x) = cos x
  tangent (Radians x) = tan x

  arcsine    x = Radians (asin x)
  arccosine  x = Radians (acos x)
  arctangent x = Radians (atan x)

instance Angle Degrees where
  sine    =    sine . radians
  cosine  =  cosine . radians
  tangent = tangent . radians

  arcsine    = degrees . arcsine
  arccosine  = degrees . arccosine
  arctangent = degrees . arctangent

data Latitude = North (Degrees Float) 
              | South (Degrees Float)
              deriving (Show, Eq)

data Longtitude = East (Degrees Float)
                | West (Degrees Float) 
                deriving (Show, Eq)

data Coord = Coord { lat :: Latitude
                   , lon :: Longtitude
                   } deriving (Eq, Show)

dms :: Integer -> Float -> Degrees Float
dms d m = Degrees $ fromIntegral (d + mc) + (m' / 60)
  where
    (mc, m') = m `divMod'` 60

degToFloat :: Degrees Float -> Float
degToFloat (Degrees f) = f

latToDeg :: Latitude -> Degrees Float
latToDeg (South d) = negate d
latToDeg (North d) = d

degToLat :: Degrees Float -> Latitude
degToLat d | d < 0 = South $ negate d
           | otherwise = North d

latToFloat :: Latitude -> Float
latToFloat = degToFloat . latToDeg

floatToLat :: Float -> Latitude
floatToLat = degToLat . Degrees

lonToDeg :: Longtitude -> Degrees Float
lonToDeg (West d) = negate d
lonToDeg (East d) = d

degToLon :: Degrees Float -> Longtitude
degToLon d | d < 0 = West $ negate d
           | d > 180 = West $ 360 - d
           | otherwise = East d

lonToFloat :: Longtitude -> Float
lonToFloat = degToFloat . lonToDeg

floatToLon :: Float -> Longtitude
floatToLon = degToLon . Degrees



-- floatToLat :: Float -> Latitude
-- floatToLat x | x < 0 = South $ Degrees $ x * (-1)
--              | otherwise = North $ Degrees x

-- floatToLon :: Float -> Longtitude
-- floatToLon x | x < 0 = East $ Degrees $ x * (-1)
--              | otherwise = West $ Degrees x