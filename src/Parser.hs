module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number
    
import Types

parseEquation :: String -> Either ParseError RawInequality
parseEquation s = parse inequality "(unknown)" s

inequality :: Parser RawInequality
inequality = do
    left <- side
    spaces
    op <- inequalityOperator
    spaces
    right <- side
    return $ RawInequality left right op

inequalityOperator :: Parser String
inequalityOperator =
    foldl1 (<|>) . map (try . string) $ ["<=", ">=", ">", "<", "="]

side :: Parser Side
side = many1 (inequalityToken <* many space)


inequalityToken :: Parser Token
inequalityToken = do
    optional $ string "+"
    spaces
    try varToken <|> constantToken

constantToken :: Parser Token
constantToken = ConstantToken <$> coeff

varToken :: Parser Token
varToken = do 
    c <- coeff
    v <- var
    return $ VarToken c v

coeff :: Parser Int
coeff = int

var :: Parser Char
var = letter
