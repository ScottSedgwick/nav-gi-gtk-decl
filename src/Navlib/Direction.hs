module Navlib.Direction 
  ( compassError        -- :: Variation    -> Deviation       -> CompassError
  , trueToMagnetic      -- :: Variation    -> TrueHeading     -> MagneticHeading
  , magneticToTrue      -- :: Variation    -> MagneticHeading -> TrueHeading
  , magneticToCompass   -- :: Deviation    -> MagneticHeading -> CompassHeading
  , compassToMagnetic   -- :: Deviation    -> CompassHeading  -> MagneticHeading
  , trueToCompass       -- :: Variation    -> Deviation       -> TrueHeading       -> CompassHeading
  , compassToTrue       -- :: Variation    -> Deviation       -> CompassHeading    -> TrueHeading
  , findCompassError    -- :: TrueHeading  -> CompassHeading  -> CompassError
  , findDeviation       -- :: CompassError -> Variation       -> Deviation
  , findVariation       -- :: CompassError -> Deviation       -> Variation
  )
where

import Navlib.Types

headingToBearing :: Heading -> Bearing
headingToBearing (Heading x) = abs $ BEast x

compassError :: Variation -> Deviation -> CompassError
compassError (Variation v) (Deviation d) = CompassError $ v + d

trueToMagnetic :: Variation -> TrueHeading -> MagneticHeading
trueToMagnetic (Variation v) (TrueHeading t) = MagneticHeading $ t - bearingToHeading v

magneticToTrue :: Variation -> MagneticHeading -> TrueHeading
magneticToTrue (Variation v) (MagneticHeading m) = TrueHeading $ m + bearingToHeading v

magneticToCompass :: Deviation -> MagneticHeading -> CompassHeading
magneticToCompass (Deviation d) (MagneticHeading m) = CompassHeading $ m - bearingToHeading d

compassToMagnetic :: Deviation -> CompassHeading -> MagneticHeading
compassToMagnetic (Deviation d) (CompassHeading c) = MagneticHeading $ c + bearingToHeading d

trueToCompass :: Variation -> Deviation -> TrueHeading -> CompassHeading
trueToCompass v d = magneticToCompass d . trueToMagnetic v

compassToTrue :: Variation -> Deviation -> CompassHeading -> TrueHeading
compassToTrue v d = magneticToTrue v . compassToMagnetic d

findCompassError :: TrueHeading -> CompassHeading -> CompassError
findCompassError (TrueHeading t) (CompassHeading c) = CompassError $ headingToBearing $ t - c

findDeviation :: CompassError -> Variation -> Deviation
findDeviation (CompassError e) (Variation v) = Deviation $ e - v

findVariation :: CompassError -> Deviation -> Variation
findVariation (CompassError e) (Deviation d) = Variation $ e - d
