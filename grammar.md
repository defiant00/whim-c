Statements
```
program     = {stmt} EOF ;

stmt        = constDecl | varDecl
            | exprStmt
            | emptyStmt ;

constDecl   = IDENTIFIER "::" expr ;
varDecl     = IDENTIFIER ":=" expr ;

exprStmt    = expr ;

emptyStmt   = ";" ;
```

Expressions
```
expr        = equality ;
equality    = comparison { ( "==" | "!=" ) comparison } ;
comparison  = term { ( "<" | "<=" | ">" | ">=" ) term } ;
term        = factor { ( "+" | "-" ) factor } ;
factor      = unary { ( "*" | "/" | "%" ) unary } ;
unary       = ( "-" | "!" ) unary
            | primary ;
primary     = "true" | "false" | "nil"
            | NUMBER | STRING | IDENTIFIER
            | "(" expr ")"
            | class
            | function ;

class       = "class" [ "is" IDENTIFIER ]
              {stmt}
              "/class" ;
function    = "fn" "(" [ IDENTIFIER { "," IDENTIFIER } ] ")"
              {stmt}
              "/fn" ;
```
