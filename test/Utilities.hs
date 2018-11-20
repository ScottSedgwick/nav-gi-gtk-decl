{-# LANGUAGE FlexibleInstances #-}
module Utilities where

import Test.HUnit.Approx (assertApproxEqual)
import Test.HUnit.Lang (Assertion)

import Navlib

class ShouldBeClose a where
  (@=?) :: a -> a -> Assertion
  (@=?) = sbc ""
  sbc :: String -> a -> a -> Assertion

instance ShouldBeClose Float where
  sbc s a b = assertApproxEqual s 0.1 b a

instance ShouldBeClose NMiles where
  sbc s a b = assertApproxEqual s (NMiles 0.1) b a

instance ShouldBeClose (Degrees Float) where
  sbc s a b = assertApproxEqual s (Degrees 0.1) b a

instance ShouldBeClose Heading where
  sbc s a b = assertApproxEqual s (Heading 0.1) b a
      
instance ShouldBeClose Latitude where
  sbc s lat1 lat2 = assertApproxEqual s 0.1 (latToFloat lat2) (latToFloat lat1)
    
instance ShouldBeClose Longtitude where
  sbc s lon1 lon2 = assertApproxEqual s 0.1 (lonToFloat lon2) (lonToFloat lon1)

instance ShouldBeClose Coord where
  sbc s c1 c2 = do
      sbc ("Latitude error: "   ++ s) (lat c1) (lat c2)
      sbc ("Longtitude error: " ++ s) (lon c1) (lon c2)
      
