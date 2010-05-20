module InputDataParser ( parseInputData ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellStyle, haskellDef )
import Char

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
symbol     = P.symbol lexer
natural    = P.natural lexer
parens     = P.parens lexer
semi       = P.semi lexer
comma      = P.comma lexer
identifier = P.identifier lexer
reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer


--------------------------------------------------------------------------------

-- Pair of real number
number_pair :: Parser (Double, Double)
number_pair = do { n1 <- number
                 ; optional comma
                 ; n2 <- number
                 ; return (n1, n2)
                 }

-- Real number
number :: Parser Double
number = lexeme (do { ds1 <- many1 digit
                    ; char '.'
                    ; ds2 <- many1 digit
                    ; return (read (ds1 ++ "." ++ ds2))
                    })


word :: Parser String
word = many1 letter

wordList :: Parser [String]
wordList = sepBy word whiteSpace


--------------------------------------------------------------------------------

-- Parse a whole input data.
inputData :: Parser I.InputData
inputData = do { gzero <- gZero
               ; gammah <- gammaHalf
               ; hmax <- hMax
               ; plt <- plot
               ; e <- end
               ; return I.InputData { I.iGZero = gzero
                                    , I.iGHalf = gammah
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



