% Ein Sudoku mit Bedingungen statt Ausprobieren (library(clpfd)): Jede
% Zelle ist eine Zahl von 1 bis 9, und in jeder Zeile, jeder Spalte und
% jedem Block sind alle verschieden.
:- use_module(library(clpfd)).

sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),
    append(Rows, Cells),
    Cells ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [A, B, C, D, E, F, G, H, I],
    blocks(A, B, C),
    blocks(D, E, F),
    blocks(G, H, I).

% blocks(Zeile1, Zeile2, Zeile3): die drei Blöcke, die in diesen drei
% Zeilen nebeneinander liegen
blocks([], [], []).
blocks([N1, N2, N3 | Rest1], [N4, N5, N6 | Rest2], [N7, N8, N9 | Rest3]) :-
    all_distinct([N1, N2, N3, N4, N5, N6, N7, N8, N9]),
    blocks(Rest1, Rest2, Rest3).

% Das Sudoku aus der Wikipedia, die leeren Zellen als _.
puzzle([[5, 3, _, _, 7, _, _, _, _],
        [6, _, _, 1, 9, 5, _, _, _],
        [_, 9, 8, _, _, _, _, 6, _],
        [8, _, _, _, 6, _, _, _, 3],
        [4, _, _, 8, _, 3, _, _, 1],
        [7, _, _, _, 2, _, _, _, 6],
        [_, 6, _, _, _, _, 2, 8, _],
        [_, _, _, 4, 1, 9, _, _, 5],
        [_, _, _, _, 8, _, _, 7, 9]]).
