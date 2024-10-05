module PrimalDualInteriorPoint (runSteps, dualGap, fPrimal, hPrimal) where

import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Constants
import WriterFunction
-- import Debug.Trace (trace)

--------------------------------------------------------------------------------------------------
  
type State        = Vector Double
type SubState     = Vector Double
type Direction    = Vector Double
type SubDirection = Vector Double

--------------------------------------------------------------------------------------------------

determineSearchDirection :: State -> Double -> Maybe (Direction)
determineSearchDirection state_k nu = maybe_direction -- state_k = vector [xk, yk ,sk]
    where 
        (xk, yk, sk) = splitState state_k

        rbk = hPrimal xk
        rck = hDual   xk yk sk

        xk_diag = diag xk
        sk_diag = diag sk

        -- left_mat_top
        -- left_mat_middle  are defined in Constants.hs
        left_mat_bottom = sk_diag ||| tr zero_mat_mn ||| xk_diag

        left_mat  = left_mat_top === left_mat_middle === left_mat_bottom
        right_vec = negate $ vjoin [rbk, rck, (xk_diag #> sk - ones * scalar nu)]
    
        maybe_direction = fmap flatten $ linearSolve left_mat $ matrix 1 $ toList right_vec

splitState :: State -> (SubState, SubState, SubState)
splitState state_k = (xk, yk, sk)
  where
    xk = subVector 0     n state_k             
    yk = subVector n     m state_k           
    sk = subVector (n+m) n state_k 

--------------------------------------------------------------------------------------------------

determineStep :: State -> Direction -> Double -> Double -> Double
determineStep state_k direction rho nu = 
            let (xk, _, sk) = splitState state_k
                (dx, _, ds) = splitState direction
                alpha_x_max = calculateMaxAlpha xk dx
                alpha_s_max = calculateMaxAlpha sk ds
                alpha_max   = min alpha_x_max alpha_s_max
            in backtrack xk dx rho nu alpha_max

backtrack :: SubState -> SubDirection -> Double -> Double -> Double -> Double
backtrack xk dx rho nu alpha_max = backtrackHelper alpha_max
    where
        backtrackHelper alpha 
            | meritFunc (xk + (scalar alpha) * dx) rho nu <= (meritFunc xk rho nu + c1 * (gradMeritFunc xk rho nu) <.> dx * alpha) = alpha
            | alpha <= eps2 = alpha
            | otherwise     = backtrackHelper (c2*alpha)
        c1 = 1e-4
        c2 = 0.5
        eps2 = 1e-5

calculateMaxAlpha :: SubState -> SubDirection -> Double
calculateMaxAlpha wk dw = 
    case calculateMaybeMaxAlpha wk dw of 
        Nothing    -> error "The maximum step was not found. The search directions (primal or dual) may be the zero vector."
        Just alpha -> alpha

calculateMaybeMaxAlpha :: SubState -> SubDirection -> Maybe (Double)
calculateMaybeMaxAlpha wk dw 
    | all (\x -> x == 0) $ toList dw = Nothing
    | otherwise                      = Just $ min 1 $ max 0 min_proportion
    where
        min_proportion = minimum $ [(e - 1) * wi/dwi | (wi, dwi) <- zip (toList wk) (toList dw), dwi < 0]

--------------------------------------------------------------------------------------------------

meritFunc :: SubState -> Double -> Double -> Double 
meritFunc x rho nu = fPrimal x + rho * (norm_2 $ hPrimal x) + nu * (entropy x) 

gradMeritFunc :: SubState -> Double -> Double -> Vector Double
gradMeritFunc x rho nu = q#>x + c + scalar rho * (tr a) #> (hPrimal x) / scalar (norm_2 $ hPrimal x) + scalar nu * (cmap (\xi -> -1/xi) x) 

fPrimal :: SubState -> Double
fPrimal x = 0.5 * (x <# q) <.> x +  c <.> x

hPrimal :: SubState -> Vector Double
hPrimal x = a #> x - b

hDual :: SubState -> SubState -> SubState -> Vector Double
hDual x y s = tr a #> y + s - q #> x - c

entropy :: SubState -> Double
entropy x = negate $ sumElements $ cmap log x

dualGap :: State -> Double
dualGap state_k = xk <.> sk /fromIntegral n
    where 
        (xk, _, sk) = splitState state_k

renewBarrierParameter :: Double -> Double
renewBarrierParameter rho = min (beta*rho) rho_max

--------------------------------------------------------------------------------------------------

runSteps :: State -> Double -> Double -> Int -> String -> IO State
runSteps state_k rho nu count path
    | count <= 0 = return state_k
    | (maximum [gap_k, primal_constraint_error, dual_constraint_error]) <= eps = return state_k
    | otherwise  = case maybe_direction of
        Nothing        -> error "direction not found"
        Just direction -> do
            writeToFile path (modifyStateVector $ show state_k)
            let alpha    = determineStep state_k direction rho nu'
                state_k' = state_k + scalar alpha * direction
            runSteps state_k' rho' nu' (count-1) path
    where
        gap_k        = dualGap state_k
        (xk, yk, sk) = splitState state_k
        primal_constraint_error = norm_2 $ hPrimal xk
        dual_constraint_error   = norm_2 $ hDual   xk yk sk

        nu'             = gap_k * gamma
        maybe_direction = determineSearchDirection state_k nu
        rho'            = renewBarrierParameter rho

--------------------------------------------------------------------------------------------------