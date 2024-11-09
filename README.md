# Toy Lang

### Sample code

```
while 7 == 7 do
    a := true;
    if 1 == 1 then
        a := false;
    else
        exit(1);
    endif
endwhile
```

### Parser AST generation

![AST](https://github.com/user-attachments/assets/944aa41d-5afc-4c0e-ab0e-dcc05c96700b)

### Compiled Instruction Set

```
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
