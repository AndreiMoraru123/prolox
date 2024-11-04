:- initialization(test_keywords).
:- initialization(test_tokens).

:- consult('../lexer.pl').

test_keywords :-
    set_prolog_flag(double_quotes, codes),
    string_codes("while a == b do ; endwhile", Codes),
    phrase(tokens(Z), Codes),
    write('output: '), writeln(Z),
    writeln('').

test_tokens :-
    set_prolog_flag(double_quotes, codes),
    string_codes("unknown", Codes),
    phrase(tokens(Z), Codes),
    write('output: '), writeln(Z),
    writeln('').
