module Eval where

import Parse
import Data.Maybe

data State = State { varTable  :: [(String,Data)]
                   , returnVal :: Data
                   , printBuf  :: String
                   }

initState :: State
initState = State [] (I32 0) ""

returns :: State -> Data -> State
returns (State varT _ printB) d = State varT d printB

writeToBuffer :: State -> String -> State
writeToBuffer (State varTable ret printB) s = State varTable ret (printB ++ s)

updateVar :: State -> (String,Data) -> State
updateVar (State varT ret printB) new = State varT' ret printB
   where 
    varT' = setKey varT new
    setKey :: [(String,Data)] -> (String,Data) -> [(String,Data)]
    setKey [] n         = [n]
    setKey (p:ps) n
       | fst p == fst n = n : ps
       | otherwise      = p : (setKey ps n)

runExprs :: [Expr] -> State -> State
runExprs [] state     = state
runExprs (e:es) state = runExprs es state'
   where
    state' = eval e state

    eval :: Expr -> State -> State
    eval (Literal n) st     = returns st n
    
    eval (Variable v) (State varT _ printB) =
        State varT (fromJust (lookup v varT)) printB
    
    eval (Is (Variable name) expr) st = 
        updateVar st (name,(returnVal $ eval expr st))
    
    eval (Plus expr1 expr2) st = returns st (plus' left right)
       where
        left = returnVal (eval expr1 st)
        right = returnVal (eval expr2 st)
        plus' :: Data -> Data -> Data
        plus' (I32 l) (I32 r) = I32 (l + r)
        plus' _ _ = error "Error: Addition is defined between Integer and Integer"

    eval (Great expr1 expr2) st = returns st (greaterThan left right)
       where
        left = returnVal (eval expr1 st)
        right = returnVal (eval expr2 st)
        greaterThan :: Data -> Data -> Data
        greaterThan (I32 l) (I32 r)
           | l > r     = I32 1
           | otherwise = I32 (-1)
        greaterThan _ _ = error "Error: can only compare two Integers"


    eval (While expr loop) st = while' $ cont $ returnVal cond
       where
        cond = (eval expr st)
        cont (I32 (-1)) = False
        cont (I32 n ) = True
        cont _        = error "Error: non valid value in loop compare"
        while' :: Bool -> State
        while' True  = let st' = runExprs loop st in
                          eval (While expr loop) st'
        while' False = st

    eval (Print expr) st = 
        writeToBuffer st $ show $ returnVal (eval expr st)

    eval _ st = st

evalState :: State -> IO()
evalState (State _ _ printB) = putStr printB