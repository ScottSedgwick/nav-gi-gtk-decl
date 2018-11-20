import DirectionSpec (directionSpec)
import HorizonSpec (horizonSpec)
import ParallelSpec (parallelSpec) 
import PlaneSpec (planeSpec)
import GreatCircleSpec (greatCircleSpec)
import CartesianSpec (cartesianSpec)

-- All tests are taken from the worked examples in "The Ship Officer's Handbook" by Capt. Khan.
-- This can be located online at http://shipofficer.com/so/wp-content/uploads/2015/02/
-- Relevant PDFs are included in the docs folder
-- Tests are named by Chapter and Exercise number.

main :: IO ()
main =  directionSpec
     >> horizonSpec
     >> parallelSpec
     >> planeSpec
     >> greatCircleSpec
     >> cartesianSpec
     >> putStrLn "Finished"