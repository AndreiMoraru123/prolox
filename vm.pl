% The Virtual Machine executes the toy code within the prolog runtime.
%
% It assembles relocatable code into absolute code containing addresses
% contains pretty print routines & routines designed to pump the code
% through the whole pipeline (lexing, parsing, codegen and assembly)

% The first argument of assemble may be a list of instructions or a
% single instruction. N0 means the current address, N means the address
% where subsequent code should start.
assemble([Code1 | Code2], N0, N) :- assemble(Code1, N0, N1), assemble(Code2, N1, N).

% For simplicity, each instruction is assumed to ocupy only one word worth of memory
assemble(instr(_,_), N0, N) :- N is N0 + 1.

% When a label is encountered, the previously uninstantiated variable contained in
% the label is unified with the current address in the assembled code.
% The label itself does not occupy scpae in the final code, so the current address
% marker is not advanced.
assemble(label(N), N, N).
assemble([], N, N).

% uninstantiated variables (leaves of the dictionary) are set void, i.e. no memory
% is allocated for them
allocate(void, N, N) :- !.

% N0 refers to the first storage address needed for the symbols in dic(*)
% N refers to the first address after the storage needed for dic(*)
% N1 is the address alocated for the placeholder name (_)
allocate(dic(_, N1, Before, After), N0, N) :- allocate(Before, N0, N1), N2 is N1 + 1, allocate(After, N2, N).

% link procedure modified to supress "instr" from output
% works with list and looks up addresses
% corresponding to names
link([], Inline, Outline, _) :- Outline is Inline, !.
link([Next | Rest], Inline, Outline, D) :- link(Next, Inline, Out_line, D) , link(Rest, Out_line, Outline, D), !.
link(instr(I, Op), Inline, Outline, D) :- directaddressing(I),
                                          reverselookup(VarName, D, Op),
                                          write(Inline), write(': '),
                                          write(I), write(' '), write(VarName), write('\n'),
                                          Outline is Inline + 1.
link(instr(I, Op), Inline, Outline, _) :- write(Inline), write(': '), write(I),
                                          write(' '), write(Op), write('\n'),
                                          Outline is Inline + 1.
link(label(_), Inline, Outline, _) :- Outline is Inline.
link(block(_), Inline, Outline, _) :- Outline is Inline.

% print the symbols corresponding to each data storage address
wdata(CurrentLine, LastLine, _) :- CurrentLine > LastLine, !.
wdata(CurrentLine, LastLine, D) :- reverselookup(Name, D, CurrentLine),
                                   write(CurrentLine),
                                   write(': Storage for '),
                                   write(Name),
                                   write('\n'),
                                   NextLine is CurrentLine + 1,
                                   wdata(NextLine, LastLine, D).

% reverselookup is used to determin the symbolic name of a given address
% The dictionary is ordered by name, not by address, so this is slow
reverselookup(_, void, _) :- !, fail.
reverselookup(Name, dic(Name, Line, _, _), Line) :- !.
reverselookup(Name, dic(_, _, Before, _), Line) :- reverselookup(Name, Before, Line).
reverselookup(Name, dic(_, _, _, After), Line) :- reverselookup(Name, After, Line).

directaddressing(load).
directaddressing(store).

run(File) :- open(File, read, Stream),
             read_file(Stream, Z1),
             close(Stream),
             phrase(tokens(Z0), Z1),
             program(Z0, _, AST),
             compilestatement(AST, D, Code, []),
             assemble(Code, 1, N0),
             N1 is N0 + 1,
             allocate(D, N1, N),
             L is N - N1,
             append(Code, [instr(halt, 0), block(L)], AbsCode),
             link(AbsCode, 1, DataStartLine, D),
             LastLine is N - 1,
             wdata(DataStartLine, LastLine, D).

