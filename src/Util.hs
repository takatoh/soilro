module Util where

--------------------------------------------------------------------------------

-- Bisection Method
-- ATTENTION:  f x1 < 0 < f x2

bisectionMethod :: (Double -> Double) -> Double -> Double -> Double
bisectionMethod f x1 x2 =
  let xm = (x1 + x2) / 2
      y = f xm
  in
  if abs y < 1.0e-12 then
    xm
  else if y < 0 then
    bisectionMethod f xm x2
  else
    bisectionMethod f x1 xm

--------------------------------------------------------------------------------
