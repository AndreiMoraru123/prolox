% GraphViz utils

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(gv)).

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
