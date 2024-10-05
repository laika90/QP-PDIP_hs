module Constants where

import Numeric.LinearAlgebra

--------------------------------------------------------------------------------------------------
-- QP

n :: Int -- the number of variables
n = 7            

m :: Int -- the number of constraints
m = 3       

a :: Matrix Double
a = (m >< n) [  1, -1,  1, -1, 1, 0, 0,
               -1,  1,  2, -2, 0, 1, 0,
               -1,  1, -3, 3, 0, 0, 1 ] 

b :: Vector Double
b = vector [2, 2, 3]                

c :: Vector Double
c = vector [-2, 2, -6, 6, 0, 0, 0]  

q :: Matrix Double
q = (n >< n) [  1, -1, -1,  1, 0, 0, 0,
               -1,  1,  1, -1, 0, 0, 0, 
               -1,  1,  2, -2, 0, 0, 0,
                1, -1, -2,  2, 0, 0, 0, 
                0,  0,  0,  0, 0, 0, 0, 
                0,  0,  0,  0, 0, 0, 0, 
                0,  0,  0,  0, 0, 0, 0 ]

--------------------------------------------------------------------------------------------------
-- some parameters 

e :: Double
e       = 0.0005 

gamma :: Double
gamma   = 0.5    

rho_max :: Double
rho_max = 1e10   

beta :: Double
beta    = 2.0    

eps :: Double
eps     = 1e-5   

--------------------------------------------------------------------------------------------------
-- the matricies used in PDIP

identity_nn :: Matrix Double
identity_nn = ident n

ones :: Vector Double
ones = konst 1 n

zero_mat_mn :: Matrix Double
zero_mat_mn = (m >< n) [1..] * 0

zero_mat_nn :: Matrix Double
zero_mat_nn = (n >< n) [1..] * 0

zero_mat_mm :: Matrix Double
zero_mat_mm = (m >< m) [1..] * 0 

left_mat_top :: Matrix Double
left_mat_top = a ||| zero_mat_mm ||| zero_mat_mn

left_mat_middle ::Matrix Double
left_mat_middle = -q ||| tr a ||| identity_nn

--------------------------------------------------------------------------------------------------
