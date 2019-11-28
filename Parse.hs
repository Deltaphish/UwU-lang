module Parse where

import qualified Lexer as Lex

data Data = I32 Integer | Str String 

instance Show Data where
    show (I32 n) = show n
    show (Str s) = s

data Expr = 
    Variable String     |
    Literal Data        |
    Is Expr Expr        |
    While Expr [Expr]   |
    EndWhile [[String]] |
    Plus Expr Expr      |
    Great Expr Expr     |
    Print Expr          |
    Nop
    deriving Show



parseFile :: String -> [Expr]
parseFile file = parse tokenize
    where
        tokenize = map words $ lines file

        parse :: [[String]] -> [Expr]

        -- Parse while loops--
        parse (("OwO":exp1):rest) =
            (While (parseStmt Nop exp1) body) : (parse after)
            where
                body = parse rest
                after = getRest $ last $ body
                getRest (EndWhile rst) = rst
        
        parse (("stawp":[]):rest) = [EndWhile rest]

        parse (s:ss) = (parseStmt Nop s) : (parse ss)
        parse [] = []

        takeRestOfStr :: [String] -> String
        takeRestOfStr (s:ss)
           | last s == '\"' = init s
           | otherwise      = s ++ " " ++ takeRestOfStr ss

        parseStmt :: Expr -> [String] -> Expr
        parseStmt before []              = before
        parseStmt before (tkn:tkns) = case Lex.lexer tkn of
            Lex.Comment           ->  Nop
            (Lex.Str s)        ->  Literal (Str s)
            (Lex.Quote strChnk)   ->  Literal (Str s) where s = strChnk ++ takeRestOfStr tkns
            (Lex.Variable name)   ->  parseStmt (Variable name) tkns
            (Lex.I32 i)           ->  parseStmt (Literal (I32 i)) tkns
            Lex.Is                ->  Is before (parseStmt Nop tkns)
            Lex.Plus              ->  Plus before (parseStmt Nop tkns)
            Lex.Great             ->  Great before (parseStmt Nop (drop 1 tkns))
            Lex.Print             ->  Print (parseStmt Nop tkns)
            _                     ->  before



