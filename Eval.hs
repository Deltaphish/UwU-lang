module Eval where

import Parse
import Data.Maybe

data State = State { varTable  :: [(String,Integer)]
                   , returnVal :: Integer
                   , printBuf  :: String
                   }

initState :: State
initState = State [] 0 ""

returns :: State -> Integer -> State
returns (State varT _ printB) n = State varT n printB

writeToBuffer :: State -> String -> State
writeToBuffer (State varTable ret printB) s = State varTable ret (printB ++ s)

updateVar :: State -> (String,Integer) -> State
updateVar (State varT ret printB) new = State varT' ret printB
   where 
    varT' = setKey varT new
    setKey :: [(String,Integer)] -> (String,Integer) -> [(String,Integer)]
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
    
    eval (Plus expr1 expr2) st = 
        returns st (returnVal (eval expr1 st) + returnVal (eval expr2 st))

    eval (Great expr1 expr2) st
       | returnVal (eval expr1 st) > returnVal (eval expr2 st) = returns st 1
       | otherwise                                             = returns st (-1)

    eval (While expr loop) st
       | returnVal (eval expr st) == 1 = let st' = runExprs loop st in
                                            eval (While expr loop) st'
       | otherwise                     = st

    eval (Print expr) st = 
        writeToBuffer st $ show $ returnVal (eval expr st)

    eval _ st = st

evalState :: State -> IO()
evalState (State _ _ printB) = putStr printB