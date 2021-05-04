import AbsLI
import Interpreter



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

prog1 = Prog (SBlock 
                [SAss (Ident "x") (EInt 1),
                 SAss (Ident "soma") (EInt 0),
                 SAss (Ident "c") (EInt 10),
                 SWhile (EVar (Ident "c")) 
                        (SBlock 
                           [SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                            SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))
                           ])
                ])

testCase1 = executeP [] prog1 == Right [("x",1),("soma",55),("c",0)]


-----------------------------------------
-----------------------------------------

prog2 = Prog (SBlock 
               [SAss (Ident "x") (EInt 0),
                STry [SAss (Ident "soma") (EInt 0), SAss (Ident "c") (EDiv (EVar (Ident "x")) (EVar (Ident "soma")))] 
                     [SAss (Ident "x") (EInt 1)] 
                     [SAss (Ident "y") (EInt 2)]
               ])
                      
{-

 {
  x = 0;
  try {
    soma = 0;
    c = x / soma;
    }
  catch {
    x = 1;
    }
  finally {
    y = 2;
    }
 }


-}                    
                      
-- executeP :: RContext -> Program  -> RContext
testCase2 = executeP [] prog2  ==  Right [("x",1),("soma",0),("y",2)]


-----------------------------------------
-----------------------------------------



{-

{
  x = 0;
  try {
    soma = 10;
    c = x / soma;
    }
  catch {
    x = 1;
    }
  finally {
    y = 2;
    }
  }

-}

prog3 = Prog (SBlock 
               [SAss (Ident "x") (EInt 0),
                STry [SAss (Ident "soma") (EInt 10),SAss (Ident "c") (EDiv (EVar (Ident "x")) (EVar (Ident "soma")))] 
                     [SAss (Ident "x") (EInt 1)] 
                     [SAss (Ident "y") (EInt 2)]
               ])

testCase3= executeP [] prog3 == Right [("x",0),("soma",10),("c",0),("y",2)]


-- uma condicao necessaria (mas nao suficiente) da implementacao eh que o valor de testSuite seja "True"
testSuite = foldl (&&) True [testCase1,testCase2,testCase3]                  