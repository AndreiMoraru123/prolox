% DCG grammar - Lexer implementation
:- use_module(library(dcg/basics)). % for whitespace

% Tokens are unique and atoms that literally match the keyword
tokens(Z) --> "while", tokens(Y), {Z = [while | Y]}.
tokens(Z) --> "do", tokens(Y), {Z = [do | Y]}.
tokens(Z) --> "if", tokens(Y), {Z = [if | Y]}.
tokens(Z) --> "then", tokens(Y), {Z = [then | Y]}.
tokens(Z) --> "else", tokens(Y), {Z = [else | Y]}.
tokens(Z) --> "end", tokens(Y), {Z = [end | Y]}.
tokens(Z) --> "exit", tokens(Y), {Z = [exit | Y]}.

% comp
tokens(Z) --> "==", tokens(Y), {Z = [== | Y]}.
tokens(Z) --> "=/=", tokens(Y), {Z = [=/= | Y]}.

% assignment
tokens(Z) --> "=", tokens(Y), {Z = [= | Y]}.

% booleans
tokens(Z) --> "true", tokens(Y), {Z = [true | Y]}.
tokens(Z) --> "false", tokens(Y), {Z = [false | Y]}.
tokens(Z) --> "and", tokens(Y), {Z = [and | Y]}.
tokens(Z) --> "or", tokens(Y), {Z = [or | Y]}.

% ignore whitespace
tokens(Z) --> white, tokens(Z).

% single characters get mapped to prolog atoms and appended
tokens(Z) --> [C], tokens(Y), {name(X, [C]), Z = [X | Y]}.

% end recursion when matching an empty list
tokens(Z) --> [], {Z = []}.
