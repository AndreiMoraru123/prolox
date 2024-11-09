% The first aguments of compilestatement may be a list of ASTs or a single AST.
% The first two entry points correcpond to the former possibility.
% The remaining entry points correcpond to the latter case.
%
% In the latter case, `compilestatement(S, D, Code, EnclosingLoopEnds) :- subgoals.`,
% means that if the subgoals are satisfied, then S is a Prolog term encapsulating an AST
% which translated to the list of assembler instructions in Code using the dictionary D,
% and the labels of the statements immediately following the enclosing loops are listed
% in EnclosingLoopEnds (from inner to outermost).

:- use_module(library(lists)).  % nth

compilestatement([], _, [], _).
compilestatement([S1 | S2], D, Code, EnclosingLoopEnds) :- compilestatement(S1, D, Code1, EnclosingLoopEnds),
                                                           compilestatement(S2, D, Code2, EnclosingLoopEnds),
                                                           append(Code1, Code2, Code).
compilestatement(assign(name(X), Value), D, [instr(loadc, V), instr(store, Addr)], _) :- lookup(X, D, Addr),
                                                                                         logic_num(Value, V).
compilestatement(other, _, [instr(nop,0)], _).
compilestatement(if(Test, Then, Else), D, Code, EnclosingLoopEnds) :-
                compileboolexpr(Test, D, TrueLabel, FalseLabel, Testcode),
                compilestatement(Then, D, Thencode, EnclosingLoopEnds),
                compilestatement(Else, D, ElseCode, EnclosingLoopEnds),
                append(Testcode, [label(TrueLabel) | Thencode], C1),
                append(C1, [instr(jump, AfterIf), label(FalseLabel) | ElseCode], C2),
                append(C2, [label(AfterIf)], Code).
compilestatement(while(Test, Do), D, Code, EnclosingLoopEnds) :-
                compileboolexpr(Test, D, TrueLabel, FalseLabel, Testcode),
                compilestatement(Do, D, Docode, [FalseLabel | EnclosingLoopEnds]),
                append([label(Loopstart) | Testcode], [label(TrueLabel) | Docode], C1),
                append(C1, [instr(jump, Loopstart), label(FalseLabel)], Code).
compilestatement(exit(K), _, [instr(jump, Label)], EnclosingLoopEnds) :- nth1(K, EnclosingLoopEnds, Label).

compileboolexpr(logicexpr(Op, X1, X2), D, TrueLabel, FalseLabel, Code) :-
                compileboolexpr(X1, D, TL, FL, Code1),
                compileboolexpr(X2, D, TrueLabel, FalseLabel, Code2),
                shortcircuit(Op, TrueLabel, FalseLabel, TL, FL, BeginArg2),
                append(Code1, [label(BeginArg2) | Code2], Code).
compileboolexpr(comparison(Op, X1, X2), _, TrueLabel, FalseLabel,
                [instr(loadc, X1), instr(subc, X2), instr(JumpIf, FalseLabel), instr(jump, TrueLabel)]) :-
                unlessop(Op, JumpIf).
compileboolexpr(name(BoolVar), D, TrueLabel, FalseLabel,
                [instr(load, Addr), instr(jumpeq, FalseLabel), instr(jump, TrueLabel)]) :-
                lookup(BoolVar, D, Addr).

shortcircuit(and, _, FalseLabel, TL, FL, BeginArg2) :- FL = FalseLabel, TL = BeginArg2.
shortcircuit(or, TrueLabel, _, TL, FL, BeginArg2) :- TL = TrueLabel, FL = BeginArg2.

unlessop(==, jumpne).
unlessop(=/=, jumpeq).

% lookup is used to "build" the symbol table during codegen,
% i.e. the second parameter is used as the output during code generation
%
% The symbol table so built has uninstantiated variables
% in place of adresses upon completion of codegen
lookup(Name, dic(Name, Value, _, _), Value) :- !.
lookup(Name, dic(Name1, _, Before, _), Value) :- Name @< Name1, lookup(Name, Before, Value).
lookup(Name, dic(Name1, _, _, After), Value) :- Name @> Name1, lookup(Name, After, Value).

% convention for representing Boolean values numerically
logic_num(true, 1).
logic_num(false, 0).

compile(File) :- open(File, read, Stream),
                 read_file(Stream, Z1),
                 close(Stream),
                 phrase(tokens(Z0), Z1),
                 program(Z0, _, AST),
                 compilestatement(AST, _, RelocatableCode, []),
                 maplist(writeln, RelocatableCode).
