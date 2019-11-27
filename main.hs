import System.Environment

import Parse
import Eval

main :: IO()
main = do
    args <- getArgs
    file <- readFile $ head args
    let ast = parseFile file
    let res = runExprs ast initState
    evalState res