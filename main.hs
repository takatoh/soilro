module Main where

import System.Environment ( getArgs )
import System.Console.GetOpt
import Text.Printf

import qualified DataDef as D
import qualified InputDataParser as P

--------------------------------------------------------------------------------

progName = "soilro"
version  = "v0.7.1"


main :: IO ()
main = do argv <- getArgs
          (o, n) <- parseArgs argv
          if optShowVersion o then
            putStrLn version
          else if optShowHelp o then
            putStrLn $ usageInfo header options
          else do cs <- readFile (head n)
                  let input = P.parseInputData cs
                  let model = genModel (optModelType o) input
                  let format = genFormatter (optOutputFormat o)
                  putStr $ format $ map model $ D.iPlotG input

--------------------------------------------------------------------------------

-- Command-line options

data Options = Options { optShowVersion  :: Bool
                       , optShowHelp     :: Bool
                       , optModelType    :: String
                       , optOutputFormat :: String
                       } deriving (Show, Eq)


defaultOptions = Options { optShowVersion  = False
                         , optShowHelp     = False
                         , optModelType    = "ro"
                         , optOutputFormat = "standard"
                         }


options :: [OptDescr (Options -> Options)]
options = [ Option []        ["csv"]
            (NoArg (\ opts -> opts { optOutputFormat = "csv" }))
            "output CSV"
          , Option []        ["shake"]
            (NoArg (\ opts -> opts { optOutputFormat = "shake" }))
            "output for SHAKE"
          , Option []        ["k-shake"]
            (NoArg (\ opts -> opts { optOutputFormat = "k-shake" }))
            "output for k-SHAKE(CSV)"
          , Option []        ["hd-model"]
            (NoArg (\ opts -> opts { optModelType = "hd" }))
            "H-D model"
          , Option []        ["modified-ro-model"]
            (NoArg (\ opts -> opts { optModelType = "mro" }))
            "modified R-O model"
          , Option ['v']     ["version"]
            (NoArg (\ opts -> opts { optShowVersion = True }))
            "show version"
          , Option ['h','?'] ["help"]
            (NoArg (\ opts -> opts { optShowHelp = True }))
            "show this message"
          ]


header :: String
header = "Usage: " ++ progName ++ " [OPTIONS...] INPUT_FILE"


parseArgs :: [String] -> IO (Options, [String])
parseArgs argv = case getOpt Permute options argv of
                   (o,n,[])   -> return (foldl (flip id) defaultOptions o, n)
                   (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

--------------------------------------------------------------------------------

-- Output formatters

genFormatter :: String -> ([(D.Gamma, D.GRatio, D.H)] -> String)
genFormatter "csv"     = formatCSV
genFormatter "shake"   = formatSHAKE
genFormatter "k-shake" = formatKSHAKE
genFormatter _         = formatOutput


formatOutput :: [(D.Gamma, D.GRatio, D.H)] -> String
formatOutput d = unlines $ header ++ map format d
  where
    format (gamma, ratio, h) = printf "   %-7f   %-7.3f   %-7.3f" gamma ratio h
    header = [ "  gamma %     G/G0       h"
             , "============================="
             ]


formatCSV :: [(D.Gamma, D.GRatio, D.H)] -> String
formatCSV d = unlines $ "gamma%,G/G0,h" : map format d
  where
    format (gamma, ratio, h) = printf "%f,%f,%f" gamma ratio h


formatSHAKE :: [(D.Gamma, D.GRatio, D.H)] -> String
formatSHAKE d = unlines $ headerRatio ++ gamma ++ ratio ++ headerH ++ gamma ++ h
  where
    headerRatio = [printf "%5d      gamma-G/G0" (length d)]
    headerH     = [printf "%5d      gamma-h" (length d)]
    gamma = map concat $ fold8 $ map (\(a,b,c) -> printf "   %-7f" a) d
    ratio = map concat $ fold8 $ map (\(a,b,c) -> printf "   %-7.3f" b) d
    h     = map concat $ fold8 $ map (\(a,b,c) -> printf "   %-7.2f" (c * 100.0)) d
    fold8 xs = let (h,t) = splitAt 8 xs
               in
               if length t == 0 then h:[] else h : fold8 t


formatKSHAKE :: [(D.Gamma, D.GRatio, D.H)] -> String
formatKSHAKE d = unlines $ "gamma%,G/G0,h%" : map format d
  where
    format (gamma, ratio, h) = printf "%f,%f,%f" gamma ratio (h * 100.0)

--------------------------------------------------------------------------------

-- Models

genModel :: String -> D.InputData -> D.Gamma -> (D.Gamma, D.GRatio, D.H)
genModel "hd"  = hdModel
genModel "mro" = modifiedRoModel
genModel _     = roModel          -- R-O model in Default


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


-- modified R-O model

modifiedRoModel :: D.InputData -> D.Gamma -> (D.Gamma, D.GRatio, D.H)
modifiedRoModel input gamma = (gamma, gRatio, h)
  where
    gammaH = D.iGHalf input
    hmax   = D.iHMax input
    beta   = (2.0 * pi * hmax) / (2.0 - pi * hmax)
    gRatio = calcGRatioModified gamma gammaH beta
    h      = 2.0 * beta / (pi * (beta + 2.0)) * (1.0 - gRatio)


calcGRatioModified :: D.Gamma -> D.Gamma -> Double -> Double
calcGRatioModified g gh b = bisectionMethod f 0.0 1.0
  where
    f x = x * (1.0 + (2.0 * x * (g / gh)) ** b) - 1.0

--------------------------------------------------------------------------------

-- Bisection Method
-- ATTENTION:  f x1 < 0 < f x2

bisectionMethod :: (Double -> Double) -> Double -> Double -> Double
bisectionMethod f x1 x2 =
  let xm = (x1 + x2) / 2
      y = f xm
  in
  if (abs y) < 0.0000000001 then
    xm
  else if y < 0 then
    bisectionMethod f xm x2
  else
    bisectionMethod f x1 xm

--------------------------------------------------------------------------------
