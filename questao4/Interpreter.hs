module Interpreter where

import AbsLI
import Prelude hiding (lookup)


type ErrorMessage = String

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
         Right n -> if n /= 0
               then case execute context stm of
                  Right ctx -> execute ctx (SWhile exp stm)
                  Left msg -> Left msg
               else Right context
   STry (t:try) catch finally -> case execute context t of
      Right ctx -> execute ctx (STry try catch finally)
      Left msg -> case execute context (SBlock catch) of
         Right ctx -> execute ctx (SBlock finally)
         Left msg -> Left msg
   STry [] _ finally -> execute context (SBlock finally)


{- Dica: o tipo de eval deve mudar para
 eval :: RContext -> Exp -> Either ErrorMessage Integer
-}
eval :: RContext -> Exp ->Either ErrorMessage Integer
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
         Right ve2 -> if ve2 == 0
            -- Retirando explicacao do erro para validar com o caso de teste. Seria mais interessante para um interpretador retornar o trech ode codigo em que foi reconhecido erro de sintaxe
            then Left "divisao por 0"
            else Right (ve1 `div` ve2)
         Left msg -> Left msg
      Left msg -> Left msg
   EInt n -> Right n
   EVar id -> Right (lookup context (getStr id))

{-  algumas dicas abaixo...para voce adaptar o codigo acima
    EDiv e1 e2 -> case eval context e1 of 
                    Right ve1 -> case eval context e2 of 
                                   Right ve2 -> if (ve2 == 0)
                                                 then Left ("divisao por 0 na expressao: " 
                                                            ++ show (EDiv e1 e2))
                                                 else Right (ve1 `div` ve2)
                                  Left msg -> Left msg  
                    Left msg -> Left msg  
    EInt n  ->  Right n 
-}


-- Dica: voce nao precisa mudar o codigo a partir daqui
type RContext = [(String,Integer)]

getStr :: Ident -> String
getStr (Ident s) = s

lookup :: RContext -> String -> Integer
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> String -> Integer -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv
