% GraphViz utils for instruction set generation

:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(gv)).

% Main export for instruction visualization
export_instruction_graph(File, Instructions) :-
    gv_export(File, {Instructions}/[Out]>>export_instructions(Out, Instructions),
              [directed(true),       % We want directed graph for control flow
               method(dot),          % Best for directed graphs
               rankdir('TB')]).      % Top to bottom layout

% Main instruction export logic
export_instructions(Out, Instructions) :-
    % First pass: create nodes for all instructions with sequential addresses
    enumerate(Instructions, 1, EnumInstr),
    maplist(create_single_node(Out), EnumInstr),
    % Second pass: create edges for control flow
    maplist(create_flow_edges(Out, EnumInstr), EnumInstr).

% Create a node for a single instruction with its address
create_single_node(Out, (Addr, Instr)) :-
    (Instr = label(Label) ->
        % For labels, create a diamond-shaped node with HTML-like label
        format(atom(NodeLabel), '<~w:Label ~w>', [Addr, Label]),
        dot_node(Out, Addr, [label(NodeLabel), shape(diamond), style(filled), fillcolor(lightgray)])
    ; Instr = instr(Op, Arg) ->
        % For instructions, create a box node with HTML-like label for line breaks
        format(atom(NodeLabel), '<~w:~w ~w>', [Addr, Op, Arg]),
        dot_node(Out, Addr, [label(NodeLabel), shape(box)])
    ).

% Create edges for different instruction types
create_flow_edges(Out, AllInstr, (Addr, Instr)) :-
    (Instr = instr(jump, Target) ->
        % Unconditional jump
        find_label_addr(AllInstr, Target, TargetAddr),
        dot_arc(Out, Addr, TargetAddr, [color(blue), penwidth(2), constraint(false)])
    ; Instr = instr(jumpne, Target) ->
        % Conditional jump (not equal)
        NextAddr is Addr + 1,
        find_label_addr(AllInstr, Target, TargetAddr),
        dot_arc(Out, Addr, TargetAddr, [color(red), label('false'), constraint(false)]),
        dot_arc(Out, Addr, NextAddr, [color(green), label('true')])
    ; Instr = instr(jumpeq, Target) ->
        % Conditional jump (equal)
        NextAddr is Addr + 1,
        find_label_addr(AllInstr, Target, TargetAddr),
        dot_arc(Out, Addr, TargetAddr, [color(red), label('true'), constraint(false)]),
        dot_arc(Out, Addr, NextAddr, [color(green), label('false')])
    ; % Regular instruction: sequential flow
        NextAddr is Addr + 1,
        (member((NextAddr, _), AllInstr) ->
            dot_arc(Out, Addr, NextAddr, [])
        ; true)
    ).

% Helper to find label address
find_label_addr(Instructions, LabelName, Addr) :-
    member((Addr, label(LabelName)), Instructions), !.

% Helper to enumerate list items with addresses
enumerate(List, Start, Enumerated) :-
    enumerate(List, Start, [], Enumerated).

enumerate([], _, Acc, Enumerated) :-
    reverse(Acc, Enumerated).

enumerate([H|T], N, Acc, Enumerated) :-
    N1 is N + 1,
    enumerate(T, N1, [(N,H)|Acc], Enumerated).
