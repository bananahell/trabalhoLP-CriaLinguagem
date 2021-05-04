import AbsLI
import Interpreter

prog1 = Prog (SAss (Ident "x") (EInt 1))
                      
{-

 x = 1;

-}                    
                      
-- executeP :: RContext -> Program  -> RContext
testCase1 = executeP [] prog1  ==  Right [("x",1)]


-----------------------------------------
-----------------------------------------


{--

{
  x = 1;
  soma = 0;
  c = 10;
  while (c) {
    soma = soma + c;
    c = c - 1;
    }
  }

--}

prog2 = Prog (SBlock 
                [SAss (Ident "x") (EInt 1),
                 SAss (Ident "soma") (EInt 0),
                 SAss (Ident "c") (EInt 10),
                 SWhile (EVar (Ident "c")) 
                        (SBlock 
                           [SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                            SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                           ])
                ])

testCase2= executeP [] prog2 == Right [("x",1),("soma",55),("c",0)]


-----------------------------------------
-----------------------------------------


{--

{
  x = 1;
  y = 0;
  z = x / y;
  w = z + 1;
}

--}

prog3 =  Prog (SBlock 
               [SAss (Ident "x") (EInt 1),
                SAss (Ident "y") (EInt 0),
                SAss (Ident "z") (EDiv (EVar (Ident "x")) (EVar (Ident "y"))),
                SAss (Ident "w") (EAdd (EVar (Ident "z")) (EInt 1))
               ])

testCase3= executeP [] prog3 == Left "divisao por 0"


-- uma condicao necessaria (mas nao suficiente) da implementacao eh que o valor de testSuite seja "True"
testSuite = foldl (&&) True [testCase1,testCase2,testCase3]                  