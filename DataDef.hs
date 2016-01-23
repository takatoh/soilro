module DataDef where


type Gamma  = Double
type G      = Double
type H      = Double
type GRatio = Double

data InputData = InputData { iGHalf :: Gamma
                           , iHMax  :: H
                           , iPlotG :: [Gamma]
                           }
               | ParseErr String
               deriving (Show, Eq)

--------------------------------------------------------------------------------

