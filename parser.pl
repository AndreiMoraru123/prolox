% Top-Down parser
%
% using the :- neck operator
% programcomponent(Z0,Z,X) :- subgoals
%
% where:
% Z0 = token to be processed (initial state)
% Z = token stream after the programcomponent (remaining state)
% X = AST list

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(gv)).

% the whole program is just a sequence of statements
program(Z0, Z, X) :- statements(Z0, Z, X).

% X0 is passed down to reststatements and returned as a prefix of X
statements(Z0, Z, X) :- statement(Z0, Z1, X0), reststatements(Z1, Z, X0, X).

% Here ; serves as a statement separator
reststatements([';' | Z0], Z, X0, Y) :- statements(Z0, Z, X), append(X0, X, Y).
reststatements(Z, Z, X, X). % base case, AST list remains unchanged


% assignment to a boolean value (e.g. x := true)
statement([V, :=, BoolVal | Z], Z, [assign(name(V), BoolVal)]) :- atom(V), boolval(BoolVal).

% if expr
statement([if | Z0], Z, [if(Expr, Then ,Else)]) :- compoundboolexpr(Z0, [then | Z1], Expr),
                                                   statements(Z1, [else | Z2], Then),
                                                   statements(Z2, [endif | Z], Else).
% while expr
statement([while | Z0], Z, [while(Expr, Do)]) :- compoundboolexpr(Z0, [do | Z1], Expr),
                                                 statements(Z1, [endwhile | Z], Do).

% exit program out of whatever NumLevels nested loops
statement([exit, '(', NumLevels, ')' | Z], Z, [exit(NumLevels)]).

% 2 is the lowest precedence level
compoundboolexpr(Z0, Z, X) :- boolsubexpr(2, Z0, Z, X).
boolsubexpr(N, Z0, Z, X) :- N > 0, N1 is N-1, boolsubexpr(N1, Z0, Z1, X0), restboolexpr(N, Z1, Z, X0, X).
boolsubexpr(0, [Digit1, Op, Digit2 | Z], Z, comparison(Op, Digit1, Digit2)) :- digit(Digit1),
                                                                               comparisonop(Op),
                                                                               digit(Digit2).
boolsubexpr(0, [X | Z], Z, name(X)) :- atom(X).

restboolexpr(N, [Op | Z0], Z, X1, X) :- logicop(N, Op), N1 is N-1,
                                        boolsubexpr(N1, Z0, Z1, X2),
                                        restboolexpr(N, Z1, Z, logicexpr(Op, X1, X2), X).
restboolexpr(_,Z, Z, X, X).

% primary exprs
boolval(true).
boolval(false).

digit(0).
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

comparisonop(==).
comparisonop(=/=).

logicop(1, and).
logicop(2, or).

parse(File) :- open(File, read, Stream),
               read_file(Stream, Z1),
               close(Stream),
               atom_codes(Content, Z1),
               write('File content: '), writeln(Content),
               phrase(tokens(Z0), Z1),
               write('Tokens: '), writeln(Z0),
               program(Z0, _, AST),
               write('AST: '), write(AST),
               export_svg('AST.svg', AST).

% File reading is funny in SWI-Prolog
read_file(Stream, Codes) :-
    read_file(Stream, [], Codes).
read_file(Stream, Acc, Codes) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, LineCodes),
    append(Acc, LineCodes, NewAcc),
    read_file(Stream, NewAcc, Codes).
read_file(Stream, Codes, Codes) :-
    at_end_of_stream(Stream).


% GraphViz utils
export_svg(File, AST) :-
    gv_export(File, {AST}/[Out]>>export_ast(Out, AST), [directed(true)]).

% main export predicate with list handling
export_ast(Out, AST) :-
    (is_list(AST) ->
        % handle lists by exporting each element
        maplist(export_ast(Out), AST)
    ; atomic(AST) ->
        % handle atomic values (numbers, atoms)
        format(atom(Label), '<~w>', [AST]),
        dot_node(Out, AST, [label(Label)])
    ; AST =.. [Op | Children] ->
        % handle compound terms (split into operator and arguments)
        format(atom(Label), '<~w>', [Op]),
        dot_node(Out, AST, [label(Label)]),
        maplist(export_children(Out, AST), Children)
    ).

% modified child export to handle lists
export_children(Out, Parent, Child) :-
    (is_list(Child) ->
        % for list children, connect parent to each element
        maplist(export_child(Out, Parent), Child)
    ; % otherwise handle normally
        dot_node(Out, Child),
        dot_arc(Out, Parent, Child),
        export_ast(Out, Child)
    ).

% base case - exporting a single child node
export_child(Out, Parent, Element) :-
    dot_node(Out, Element),
    dot_arc(Out, Parent, Element),
    export_ast(Out, Element).


