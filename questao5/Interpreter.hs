module Interpreter where

import AbsLI
import Prelude hiding (lookup)


type ErrorMessage = String


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

{- Dica: somente o tipo de executeP precisa mudar conforme a sugestao abaixo, 
   mas a sua definicao (corpo) pode ficar a mesma
   executeP :: RContext -> Program  -> Either ErrorMessage RContext
-}
executeP :: RContext -> Program  -> Either ErrorMessage RContext
executeP context (Prog stm) = execute context stm

{- Dica: o tipo de execute deve mudar para 
 execute :: RContext -> Stm -> Either ErrorMessage RContext   
 Alem disso, o corpo dessa funcao deve ser modificado, pois eval
 retorna tipo diferente agora, ou seja, a avaliacao pode falhar
 e, consequentemente o execute tambem. Assim, todos tipos de comandos 
 serao afetados
-}
execute :: RContext -> Stm -> Either ErrorMessage RContext
execute context x = case x of
   SAss id exp -> case eval context exp of
      Right v -> Right (update context (getStr id) v)
      Left msg -> Left msg
   SBlock [] -> Right context
   SBlock (s:stms) -> case execute context s of
      Right ctx -> execute ctx (SBlock stms)
      Left  msg -> Left msg
   SWhile exp stm -> case eval context exp of
         Right n -> if n /= ValorInt 0
               then case execute context stm of
                  Right ctx -> execute ctx (SWhile exp stm)
                  Left msg -> Left msg
               else Right context

   SdoWhile stm exp -> case execute context stm of
      Right ctx -> case eval ctx exp of
         Right i -> if i== ValorInt 0
            then execute context stm
            else case execute context stm of
               Right ctx -> execute ctx (SdoWhile stm exp)
         Left msg -> Left msg
      Left msg -> Left msg

   STry (t:try) catch finally -> case execute context t of
      Right ctx -> execute ctx (STry try catch finally)
      Left msg -> case execute context (SBlock catch) of
         Right ctx -> execute ctx (SBlock finally)
         Left msg -> Left msg
   STry [] _ finally -> execute context (SBlock finally)

{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp ->Either ErrorMessage Valor
eval context x = case x of
   EAdd exp0 exp  -> case eval context exp0 of
      Right ve1 -> case eval context exp of
         Right ve2 -> Right (ve1 + ve2)
         Left msg -> Left  msg
      Left msg -> Left msg
   ESub exp0 exp  -> case eval context exp0 of
      Right ve1 -> case eval context exp of
         Right ve2 -> Right (ve1 - ve2)
         Left msg -> Left  msg
      Left msg -> Left msg
   EMul exp0 exp  -> case eval context exp0 of
      Right ve1 -> case eval context exp of
         Right ve2 -> Right (ve1 * ve2)
         Left msg -> Left  msg
      Left msg -> Left msg
   EDiv exp0 exp -> case eval context exp0 of
      Right ve1 -> case eval context exp of
         Right ve2 -> if ve2 == ValorInt 0
            then Left "divisao por 0"
            else Right ValorInt ( i ve1 `div` i ve2)
         Left msg -> Left msg
      Left msg -> Left msg
   EInt n -> Right (ValorInt n)
   EVar id -> Right (lookup context (getStr id))

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
