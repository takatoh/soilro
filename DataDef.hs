module DataDef where



type Gamma = Double
type G     = Double
type H     = Double

data InputData = InputData { iGZero :: G
                           , iGHalf :: Gamma
                           , iHMax  :: H
                           , iPlotG :: [Gamma]
                           }
               | End
               | ParseErr String
               deriving (Show, Eq)

--------------------------------------------------------------------------------

