:- module(xmas_patterns_counter, [main/0, read_grid/2, count_all_xmas_patterns/2]).

% Read the grid from the input file
read_grid(FilePath, Grid) :-
    setup_call_cleanup(
        open(FilePath, read, Stream),
        read_lines(Stream, Grid),
        close(Stream)
    ).

read_lines(Stream, []) :-
    at_end_of_stream(Stream).
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    read_lines(Stream, Lines).

% Count all X-MAS patterns in the grid
count_all_xmas_patterns(Grid, Count) :-
    length(Grid, Rows),
    nth0(0, Grid, FirstRow),
    string_length(FirstRow, Cols),
    findall(1, (
        between(2, Rows, R),
        between(2, Cols, C),
        is_xmas_pattern(Grid, R, C)
    ), Matches),
    length(Matches, Count).

% Check if an X-MAS pattern exists at (R, C)
is_xmas_pattern(Grid, R, C) :-
    get_char_at(Grid, R, C, 'A'),
    (
        (get_char_at(Grid, R-1, C-1, 'M'),
         get_char_at(Grid, R-1, C+1, 'S'),
         get_char_at(Grid, R+1, C-1, 'M'),
         get_char_at(Grid, R+1, C+1, 'S'));
        (get_char_at(Grid, R-1, C-1, 'S'),
         get_char_at(Grid, R-1, C+1, 'M'),
         get_char_at(Grid, R+1, C-1, 'S'),
         get_char_at(Grid, R+1, C+1, 'M'));
        (get_char_at(Grid, R-1, C-1, 'M'),
         get_char_at(Grid, R-1, C+1, 'M'),
         get_char_at(Grid, R+1, C-1, 'S'),
         get_char_at(Grid, R+1, C+1, 'S'));
        (get_char_at(Grid, R-1, C-1, 'S'),
         get_char_at(Grid, R-1, C+1, 'S'),
         get_char_at(Grid, R+1, C-1, 'M'),
         get_char_at(Grid, R+1, C+1, 'M'))
    ).

% Get character at (R, C), considering the grid as a list of strings
get_char_at(Grid, RowIndex, ColIndex, Char) :-
    RowIndex > 0, ColIndex > 0, % Ensure indices are valid
    nth1(RowIndex, Grid, Row),  % Get the corresponding row
    Col is ColIndex - 1,        % Adjust for 0-based indexing in sub_string
    sub_string(Row, Col, 1, _, Char).


% Main function
main :-
    FilePath = 'input.txt',
    (   read_grid(FilePath, Grid)
    ->  (   Grid = []
        ->  writeln('Error: Grid is empty or invalid.')
        ;   count_all_xmas_patterns(Grid, XmasPatterns),
            format('Total X-MAS patterns: ~w~n', [XmasPatterns])
        )
    ;   writeln('Error: Failed to read the file.')
    ).
