import Text.Read

data Expr = 
    Variable String   |
    Literal Integer   |
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
    | s == "stowp"    = True
    | s == "*"        = True
    | s == "*Notices" = True
    | s == "nuzzels"  = True
    | otherwise       = False

parseFile :: String -> [Expr]
parseFile file = parse tokenize
    where
        tokenize = map words $ lines file

        parseToken :: String -> Expr
        parseToken s = parseTken (readMaybe s :: Maybe Integer)
           where
            parseTken (Just n) = Literal n
            parseTken Nothing
                | isReserved s = error "Use of reserved word"
                | otherwise    = Variable s


        parse :: [[String]] -> [Expr]
        
        parse (("OwO":exp1):rest) =
            (While (parseStmt exp1) body) : (parse after)
            where
                body = map parseStmt $ takeWhile (\t -> t /= ["stowp"]) rest
                after = drop (length body+1) rest
        
        parse (s:ss) = (parseStmt s) : (parse ss)
        parse [] = []

        parseStmt :: [String] -> Expr

        parseStmt [] = Nop

        parseStmt ("nuzzels":expr) = 
            Print (parseStmt expr)

        parseStmt ("*notices":tkn1:"is":"gweatew":"than":tkn2:"*":[]) =
            Great (parseToken tkn1) (parseToken tkn2)

        parseStmt (tkn:"pwus":exp2) =
            Plus (parseToken tkn) (parseStmt exp2)

        parseStmt (var:"iws":exp) =
            Is (Variable var) (parseStmt exp)

        parseStmt (tkn:[]) = parseToken tkn 


