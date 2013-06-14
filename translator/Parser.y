{
module Main where
import Token
}

%name parse
%tokentype {Token}
%error {parseError}

%token 
    generate {Generate}
    queue {Queue}
    seize {Seize}
    depart {Depart}
    advance {Advance}
    release {Release}
    terminate {Terminate}
    ',' {Comma}
    label {Label $$}
    num {Num $$}
    
%%

Prog  : Statement {"\t" ++ $1}
      | Statement Prog {"\t" ++ $1 ++ "\n" ++ $2} 
      
Statement : generate num {"generate" ++ show $2 }
          | generate num ',' num {"generate ("++show $2 ++ "," ++ show $4 ++")"}
          | generate ',' num {"generate (()," ++ show $3 ++ ")"}
          | queue label {"queue " ++ show $2 }
          | seize label {"seize " ++ show $2 }
          | depart label {"depart " ++ show $2}
          | advance num {"advance " ++ show $2}
          | advance num ',' num {"advance ("++show $2 ++ "," ++ show $4 ++ ")"}
          |advance ',' num {"advance (()," ++ (show $3) ++ ")"}
          |release label {"release "++ show $2}
          | terminate  {"terminate "}
          | terminate num {"terminate "++(show $2)}

            
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

translate s = putStrLn ("model = do\n" ++ (parse $ alexScanTokens s))

main = do s <- getContents
          translate s
}
