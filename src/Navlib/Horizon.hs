module Navlib.Horizon
  ( visibleHorizon
  , radarHorizon
  ) 
  where

import Navlib.Types

visibleHorizon :: Height -> NMiles
visibleHorizon (Feet f)   = NMiles $ 1.17 * sqrt f
visibleHorizon (Metres m) = NMiles $ 2.07 * sqrt m

radarHorizon :: Height -> NMiles
radarHorizon (Feet f)   = NMiles $ 1.22 * sqrt f
radarHorizon (Metres m) = NMiles $ 2.21 * sqrt m
