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

