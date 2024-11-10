% Top-Down parser
%
% using the :- neck operator
% programcomponent(Z0,Z,X) :- subgoals
%
% where:
% Z0 = token to be processed (initial state)
% Z = token stream after the programcomponent (remaining state)
% X = AST list

:- consult('utils/read_file.pl').
:- consult('utils/export_ast_graph.pl').

% the whole program is just a sequence of statements
program(Z0, Z, X) :- statements(Z0, Z, X).

% base case - succeed on encountering explicit terminators
statements(Z, Z, []) :-
    (Z = [end|_] ; Z = [else|_] ; Z = []).

% Handle statement lists avoiding double nesting (nested trees)
statements(Z0, Z, Stmts) :-
    \+ (Z0 = [end|_] ; Z0 = [else|_] ; Z0 = []),
    statement(Z0, Z1, Stmt),
    (   Z1 = [';'|Rest] % handle whether there is a semicolon or not
    ->  statements(Rest, Z, RestStmts),
        % if Stmt is already a list [X], extract X and put it in the result
        (Stmt = [X] -> Stmts = [X | RestStmts] ; Stmts = [Stmt | RestStmts])
    ;   statements(Z1, Z, RestStmts),
        (Stmt = [X] -> Stmts = [X | RestStmts] ; Stmts = [Stmt | RestStmts])
).

% assignment to a boolean value (e.g. x := true)
statement([V, :=, BoolVal | Z], Z, [assign(name(V), BoolVal)]) :- atom(V), boolval(BoolVal).

% if expr
statement([if | Z0], Z, [if(Expr, Then ,Else)]) :- compoundboolexpr(Z0, [then | Z1], Expr),
                                                   statements(Z1, [else | Z2], Then),
                                                   statements(Z2, [end | Z], Else).
% while expr
statement([while | Z0], Z, [while(Expr, Do)]) :- compoundboolexpr(Z0, [do | Z1], Expr),
                                                 statements(Z1, [end | Z], Do).

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
