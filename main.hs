import Parse
import Eval

main :: IO()
main = do
    file <- readFile "Test.uwu"
    let ast = parseFile file
    let res = runExprs ast initState
    evalState res