module Model where

import qualified DataDef as D
import Util (bisectionMethod)

--------------------------------------------------------------------------------

-- Models

genModel :: String -> Either String (D.InputData -> D.Gamma -> (D.Gamma, D.GRatio, D.H))
genModel "hd"         = Right hdModel
genModel "hyperbolic" = Right hyperbolicModel
genModel "ro"         = Right roModel
genModel _            = Left "Invalid model name"


-- R-O model

roModel :: D.InputData -> D.Gamma -> (D.Gamma, D.GRatio, D.H)
roModel input gamma = (gamma, gRatio, h)
  where
    gammaH = D.iGHalf input
    hmax   = D.iHMax input
    beta   = (2.0 + pi * hmax) / (2.0 - pi * hmax)
    gRatio = calcGRatio gamma gammaH beta
    h      = hmax * (1 - gRatio)


calcGRatio :: D.Gamma -> D.Gamma -> Double -> Double
calcGRatio g gh b = bisectionMethod f 0.0 1.0
  where
    f x = x * (1.0 + (2.0 * x * (g / gh)) ** (b - 1.0)) - 1.0


-- H-D model

hdModel :: D.InputData -> D.Gamma -> (D.Gamma, D.GRatio, D.H)
hdModel input gamma = (gamma, gRatio, h)
  where
    gammaH = D.iGHalf input
    hmax   = D.iHMax input
    gRatio = 1.0 / (1.0 + gamma / gammaH)
    h      = hmax * (1 - gRatio)


-- Hyperbolic model

hyperbolicModel :: D.InputData -> D.Gamma -> (D.Gamma, D.GRatio, D.H)
hyperbolicModel input gamma = (gamma, gRatio, h)
  where
    gammaH = D.iGHalf input
    gRatio = 1.0 / (1.0 + gamma / gammaH)
    h      = 4.0 / pi * (1.0 + ggh) * (1.0 - ggh * log (1.0 + 1.0 / ggh)) - 2.0 / pi
    ggh    = gammaH / gamma

--------------------------------------------------------------------------------
