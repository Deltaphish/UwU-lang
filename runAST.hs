import Parse

runExprs :: [Expr] -> [(String,Int)] -> IO()
runExprs (e:es) state = runExpr es state'
   where
    state' = eval e state