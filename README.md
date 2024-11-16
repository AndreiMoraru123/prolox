# Toy Lang

### Sample code

```lua
while 7 == 7 do
    a = true;
    if 1 == 1 then
        a = false;
    else
        exit(1);
    end
end
```
> [!TIP]
> Run the whole pipeline via loading all the modules:
> ```sh
> swipl lexer.pl parser.pl compiler.pl vm.pl
> ```

### Parser AST generation

```pl
?- parse('examples/while_loop.toy').
```

![AST](https://github.com/user-attachments/assets/944aa41d-5afc-4c0e-ab0e-dcc05c96700b)

### Compiled Instruction Set

```pl
?- compile('examples/while_loop.toy').
```

```antlr
label(_53158)
instr(loadc,7)
instr(subc,7)
instr(jumpne,_52674)
instr(jump,_52672)
label(_52672)
instr(loadc,1)
instr(store,_52758)
instr(loadc,1)
instr(subc,1)
instr(jumpne,_52776)
instr(jump,_52774)
label(_52774)
instr(loadc,0)
instr(store,_52758)
instr(jump,_52944)
label(_52776)
instr(jump,_52674)
label(_52944)
instr(jump,_53158)
label(_52674)
```

### Assembled VM Code

```pl
?- run('examples/while_loop.toy').
```

```antlr
1: loadc 7
2: subc 7
3: jumpne 16
4: jump 5
5: loadc 1
6: store a
7: loadc 1
8: subc 1
9: jumpne 14
10: jump 11
11: loadc 0
12: store a
13: jump 15
14: jump 16
15: jump 1
16: halt 0
17: Storage for a
```

### Appendinx

#### Backus-Naur Form (BNF) Grammar

##### Syntax Grammar

```antlr
// Statements

program        → statement* ;
statement      → assignment
               | ifStmt
               | whileStmt 
               | exitStmt ;

assignment     → IDENTIFIER "=" boolValue (";" )? ;
ifStmt         → "if" comparison "then" statement* 
                 ( "else" statement* )? "end" ;
whileStmt      → "while" comparison "do" statement* "end" ;
exitStmt       → "exit" "(" NUMBER ")" (";" )? ;

// Expressions

comparison     → NUMBER ( "==" | "=/=" ) NUMBER ;
boolValue      → "true" | "false" ;
```
##### Lexical Grammar

```antlr
NUMBER         → DIGIT ;
IDENTIFIER     → ALPHA ;
ALPHA          → "a" ... "z" | "A" ... "Z" ;
DIGIT          → "0" ... "9" ;
```
