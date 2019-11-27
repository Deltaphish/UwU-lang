module Parse where

import Text.Read

data Data = I32 Integer | Str String 

instance Show Data where
    show (I32 n) = show n
    show (Str s) = s

data Expr = 
    Variable String   |
    Literal Data      |
    Is Expr Expr      |
    While Expr [Expr] |
    EndWhile          |
    Plus Expr Expr    |
    Great Expr Expr   |
    Print Expr        |
    Nop
    deriving Show

isReserved :: String -> Bool
isReserved s
    | s == "iws"      = True
    | s == "pwus"     = True
    | s == "stawp"    = True
    | s == "*"        = True
    | s == "*notices" = True
    | s == "nuzzels"  = True
    | otherwise       = False

parseFile :: String -> [Expr]
parseFile file = parse tokenize
    where
        tokenize = map words $ lines file

        parseToken :: String -> Expr
        parseToken s = parseTken (readMaybe s :: Maybe Integer)
           where
            parseTken (Just n) = Literal (I32 n)
            parseTken Nothing
                | isReserved s = error "Use of reserved word"
                | otherwise    = Variable s


        parse :: [[String]] -> [Expr]
        
        parse (("OwO":exp1):rest) =
            (While (parseStmt Nop exp1) body) : (parse after)
            where
                body = map (parseStmt Nop) $ takeWhile (\t -> t /= ["stawp"]) rest
                after = drop (length body+1) rest
        
        parse (s:ss) = (parseStmt Nop s) : (parse ss)
        parse [] = []

        lexer :: String -> (String,Maybe Integer)
        lexer ('\"':xs) = ("Quote",Nothing)
        lexer "*"       = ("",Nothing)
        lexer "iws"     = ("Is",Nothing)
        lexer "pwus"    = ("Plus",Nothing)
        lexer "nuzzels" = ("Print",Nothing)
        lexer "gweatew" = ("Great",Nothing)
        lexer tkn       = 
            case (readMaybe tkn :: Maybe Integer) of
                 Just n  -> ("Literal",Just n)
                 Nothing -> ("Variable",Nothing)



        takeRestOfStr :: [String] -> String
        takeRestOfStr (s:ss)
           | last s == '\"' = init s
           | otherwise      = s ++ " " ++ takeRestOfStr ss

        parseStmt :: Expr -> [String] -> Expr
        parseStmt before []              = before
        parseStmt _ (('U':'w':'U':s):ss) = Nop
        parseStmt before (tkn:tkns) = case lexer tkn of
            ("Quote", Nothing)   ->  Literal (Str $ (tail tkn) ++ " " ++ takeRestOfStr tkns)  
            ("Variable",Nothing) ->  parseStmt (Variable tkn) tkns
            ("Literal",Just n)   ->  parseStmt (Literal (I32 n)) tkns
            ("Is", Nothing)      ->  Is before (parseStmt Nop tkns)
            ("Plus", Nothing)    ->  Plus before (parseStmt Nop tkns)
            ("Great", Nothing)   ->  Great before (parseStmt Nop (drop 1 tkns))
            ("Print", Nothing)   ->  Print (parseStmt Nop tkns)
            _                    ->  before



