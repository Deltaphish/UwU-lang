module Lexer where

import Text.Read

data Token = 
    Comment         |
    Str String      |
    Quote String    |
    Variable String |
    I32 Integer     |
    Is              |
    Plus            |
    Great           |
    Print           |
    Nop
    

lexer :: String -> Token
lexer ('\"':xs) 
   | last xs == '\"' = Str $ init xs
   | otherwise       = Quote xs
lexer "UwU"          = Comment
lexer "*"            = Nop
lexer "iws"          = Is
lexer "pwus"         = Plus
lexer "nuzzels"      = Print
lexer "gweatew"      = Great

-- If not keyword, check if variable or integer
lexer tkn = 
    case (readMaybe tkn :: Maybe Integer) of
         Just n  -> I32 n
         Nothing -> Variable tkn