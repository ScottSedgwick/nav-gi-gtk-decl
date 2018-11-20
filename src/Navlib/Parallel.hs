module Navlib.Parallel where

import Navlib.Types

dlong :: Longtitude -> Longtitude -> Float
dlong lon1 lon2 = 60 * dc
  where 
    dl = abs $ lonToFloat lon2 - lonToFloat lon1
    dc = if dl > 180 then 360 - dl else dl

departure :: Latitude -> Longtitude -> Longtitude -> NMiles
departure lat1 lon1 lon2 = NMiles $ dlong lon1 lon2 * cosine (latToDeg lat1)