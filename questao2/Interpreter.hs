module Interpreter where

import AbsLI
import Prelude hiding (lookup)


executeP :: RContext -> Program  -> RContext
executeP context (Prog stm) = execute context stm
   
execute :: RContext -> Stm -> RContext
execute context x = case x of
   SAss id exp -> update context (getStr id) (eval context exp)
   SBlock [] -> context
   SBlock (s:stms) -> execute (execute context s) (SBlock stms) 
   SWhile exp stm -> if ( i(eval context exp) /= 0) 
                      then execute (execute context stm) (SWhile exp stm)
                      else context


data Valor = ValorStr String |
             ValorInt Integer |
             ValorBool Bool
-- note que ja foi adicionado um novo contrutor de tipo para valor booleano

s :: Valor -> String             
s (ValorStr str) = str
i :: Valor -> Integer
i (ValorInt vint) = vint 
-- a funcao "b" abaixo recupera o valor booleano dentro de um valor
b (ValorBool vbool) = vbool


instance Show Valor where
 show (ValorInt vint) = show vint
 show (ValorStr vstr) = vstr
 show (ValorBool vb) = show vb

-- precisamos que Valor esteja em Eq para podermos especificar os casos de teste em Testes.hs
instance Eq Valor where
 (ValorInt i1) == (ValorInt i2) =  i1 == i2
 (ValorStr s1) == (ValorStr s2) =  s1 == s2
 (ValorBool b1) == (ValorBool b2) = b1 == b2


-- note que a funcao eval retorna um Valor             
eval :: RContext -> Exp -> Valor
eval context x = case x of 
    EAdd exp0 exp -> ValorInt ( i(eval context exp0) + i (eval context exp))
    ESub exp0 exp -> ValorInt ( i(eval context exp0) - i (eval context exp))
    EMul exp0 exp -> ValorInt ( i(eval context exp0) * i (eval context exp))
    EDiv exp0 exp -> ValorInt ( i(eval context exp0) `div` i (eval context exp))
    ECon exp0 exp -> ValorStr ( s(eval context exp0) ++ s(eval context exp))
    EInt n  -> ValorInt n
    EVar id  -> lookup context (getStr id)
    EStr str -> ValorStr str
    -- adicione abaixo um padrao e comportamento associado a expressao Or
    EOr  exp0 exp -> ValorBool ( b (eval context exp0) || b (eval context exp))
    -- adicione abaixo um padrao e comportamento associado a expressao And
    EAnd exp0 exp -> ValorBool ( b (eval context exp0) && b (eval context exp))
    -- adicione abaixo um padrao e comportamento associado a expressao Not
    ENot exp -> ValorBool ( not (b (eval context exp)))
    -- adicione abaixo um padrao e comportamento associado ao literal true
    ETrue -> ValorBool True
    -- adicione abaixo um padrao e comportamento associado ao literal false
    EFalse -> ValorBool False
    
type RContext = [(String,Valor)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv
