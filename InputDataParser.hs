module InputDataParser ( parseInputData ) where

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language( haskellStyle, haskellDef )
import Data.Char

import qualified DataDef as I


--------------------------------------------------------------------------------

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        ( haskellStyle
        { P.reservedNames = [ "*G0"
                            , "*GAMMA0.5"
                            , "*HMAX"
                            , "*PLOT"
                            , "*END"
                            ]
        })

whiteSpace = P.whiteSpace lexer
lexeme     = P.lexeme lexer
reserved   = P.reserved lexer


--------------------------------------------------------------------------------

-- Real number
number :: Parser Double
number = lexeme (do { ds1 <- many1 digit
                    ; char '.'
                    ; ds2 <- many1 digit
                    ; return (read (ds1 ++ "." ++ ds2))
                    })


--------------------------------------------------------------------------------

-- Parse a whole input data.
inputData :: Parser I.InputData
inputData = do { optional gZero
               ; gammah <- gammaHalf
               ; hmax <- hMax
               ; plt <- plot
               ; e <- end
               ; return I.InputData { I.iGHalf = gammah
                                    , I.iHMax  = hmax
                                    , I.iPlotG = plt
                                    }
               }

-- G0
gZero :: Parser I.G
gZero = do { reserved "*G0"
           ; g <- number
           ; return g
           }

-- Gamma0.5
gammaHalf :: Parser I.Gamma
gammaHalf = do { reserved "*GAMMA0.5"
               ; g <- number
               ; return g
               }

-- Hmax
hMax :: Parser I.H
hMax = do { reserved "*HMAX"
          ; h <- number
          ; return h
          }


-- *PLOT
plot :: Parser [I.Gamma]
plot = do { reserved "*PLOT"
          ; p <- many1 number
          ; return p
          }
     <?> "Error: PLOT"


-- *END
end :: Parser ()
end = do { reserved "*END"
         ; return ()
         }


--------------------------------------------------------------------------------

run :: Parser I.InputData -> String -> I.InputData
run p input = case (parse p "" input) of
              Left err -> I.ParseErr (show err)
              Right x  -> x

runLex :: Parser I.InputData -> String -> I.InputData
runLex p input = run (do { whiteSpace
                         ; x <- p
                         ; eof
                         ; return x
                         }) input

parseInputData :: String -> I.InputData
parseInputData = runLex inputData


--------------------------------------------------------------------------------
