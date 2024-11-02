:- initialization(run_test).

:- consult('../lexer.pl').

run_test :-
    set_prolog_flag(double_quotes, codes),
    string_codes("while a == b do other ; other endwhile", Codes),
    phrase(tokens(Z), Codes),
    write('output: '), writeln(Z),
    writeln('').
