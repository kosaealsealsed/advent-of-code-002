:- module(xmas_counter, [main/0]).

% Read the grid from the input file
read_grid(FilePath, Grid) :-
    setup_call_cleanup(
        open(FilePath, read, Stream),
        read_lines(Stream, Grid),
        close(Stream)
    ).

read_lines(Stream, Lines) :-
    read_string(Stream, _, Content),
    split_string(Content, "\n", "\s\t", Lines).

% Check if a word exists in a given direction
check_word(Grid, X, Y, Dx, Dy, Word) :-
    string_length(Word, Len),
    length(Grid, Rows),
    nth0(0, Grid, FirstRow),
    string_length(FirstRow, Cols),
    check_word(Grid, X, Y, Dx, Dy, Word, Len, Rows, Cols, 0).

check_word(_, _, _, _, _, _, Len, _, _, Len).
check_word(Grid, X, Y, Dx, Dy, Word, Len, Rows, Cols, I) :-
    Nx is X + I * Dx,
    Ny is Y + I * Dy,
    Nx >= 0, Ny >= 0, Nx < Rows, Ny < Cols,
    nth0(Nx, Grid, Row),
    sub_atom(Row, Ny, 1, _, GridChar),
    sub_atom(Word, I, 1, _, WordChar),
    GridChar == WordChar,
    NextI is I + 1,
    check_word(Grid, X, Y, Dx, Dy, Word, Len, Rows, Cols, NextI).

% Count occurrences of the word "XMAS" in all directions
count_xmas(Grid, Count) :-
    TargetWord = "XMAS",
    Directions = [(0, 1), (1, 0), (1, 1), (1, -1), (0, -1), (-1, 0), (-1, -1), (-1, 1)],
    findall(1, (
        nth0(R, Grid, _),
        nth0(C, Grid, _),
        member((Dx, Dy), Directions),
        check_word(Grid, R, C, Dx, Dy, TargetWord)
    ), Matches),
    length(Matches, Count).

% Count all X-MAS patterns in the grid
count_all_xmas_patterns(Grid, Count) :-
    findall(1, (
        nth0(R, Grid, _),
        nth0(C, Grid, _),
        valid_xmas_pattern(Grid, R, C)
    ), Matches),
    length(Matches, Count).

valid_xmas_pattern(Grid, R, C) :-
    R > 0, C > 0,
    length(Grid, Rows),
    nth0(0, Grid, FirstRow),
    string_length(FirstRow, Cols),
    R < Rows - 1, C < Cols - 1,  % Ensure bounds for neighbors
    R1 is R - 1,  % Compute row above
    R2 is R + 1,  % Compute row below
    C1 is C - 1,  % Compute column left
    C2 is C + 1,  % Compute column right
    (
        % M.S \n A \n M.S
        (nth0(R1, Grid, Row1), sub_atom(Row1, C1, 1, _, 'M'), sub_atom(Row1, C2, 1, _, 'S'),
         nth0(R, Grid, Row2), sub_atom(Row2, C, 1, _, 'A'),
         nth0(R2, Grid, Row3), sub_atom(Row3, C1, 1, _, 'M'), sub_atom(Row3, C2, 1, _, 'S'));
        % S.M \n A \n S.M
        (nth0(R1, Grid, Row1), sub_atom(Row1, C1, 1, _, 'S'), sub_atom(Row1, C2, 1, _, 'M'),
         nth0(R, Grid, Row2), sub_atom(Row2, C, 1, _, 'A'),
         nth0(R2, Grid, Row3), sub_atom(Row3, C1, 1, _, 'S'), sub_atom(Row3, C2, 1, _, 'M'));
        % M.M \n A \n S.S
        (nth0(R1, Grid, Row1), sub_atom(Row1, C1, 1, _, 'M'), sub_atom(Row1, C2, 1, _, 'M'),
         nth0(R, Grid, Row2), sub_atom(Row2, C, 1, _, 'A'),
         nth0(R2, Grid, Row3), sub_atom(Row3, C1, 1, _, 'S'), sub_atom(Row3, C2, 1, _, 'S'));
        % S.S \n A \n M.M
        (nth0(R1, Grid, Row1), sub_atom(Row1, C1, 1, _, 'S'), sub_atom(Row1, C2, 1, _, 'S'),
         nth0(R, Grid, Row2), sub_atom(Row2, C, 1, _, 'A'),
         nth0(R2, Grid, Row3), sub_atom(Row3, C1, 1, _, 'M'), sub_atom(Row3, C2, 1, _, 'M'))
    ).



% Main function
main :-
    FilePath = "input.txt",
    read_grid(FilePath, Grid),
    (Grid = [] ->
        writeln("Error: Grid is empty or invalid.");
        (
            count_xmas(Grid, XmasCount),
            format("Count of XMAS: ~w~n", [XmasCount]),
            count_all_xmas_patterns(Grid, XmasPatterns),
            format("Total X-MAS patterns: ~w~n", [XmasPatterns])
        )
    ).
