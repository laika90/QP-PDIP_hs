module Main (main) where

import Constants
import PrimalDualInteriorPoint
import WriterFunction
import Numeric.LinearAlgebra
import System.Directory (removeFile)

main :: IO ()
main = do
    removeFile log_path_state
    final_state <- runSteps initial_state rho nu max_iteration log_path_state
    print final_state
    where
        x0            = [1,2,3,4,5,6,7]             :: [Double]
        y0            = [-1,-1,-1]                 :: [Double]
        s0            = [1,1,1,1,1,1,1]             :: [Double]
        rho           = 10                      :: Double
        initial_state = vector $ x0 ++ y0 ++ s0 :: Vector Double
        nu            = dualGap initial_state
        max_iteration = 1000
        log_path_state       = "data/state.dat"
        log_path_constraints = "data/constraints.dat"