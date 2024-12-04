% Read the grid from a file and store it as a list of lists
read_grid(FilePath, Grid) :-
    open(FilePath, read, Stream),
    read_lines(Stream, Grid),
    close(Stream).

read_lines(Stream, []) :-
    at_end_of_stream(Stream), !.
read_lines(Stream, [Line|Lines]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, LineString),
    string_chars(LineString, Line),
    read_lines(Stream, Lines).

% Define the directions for moving in the grid
direction(0, 1).   % Right
direction(1, 0).   % Down
direction(1, 1).   % Diagonal-right-down
direction(1, -1).  % Diagonal-left-down
direction(0, -1).  % Left
direction(-1, 0).  % Up
direction(-1, -1). % Diagonal-left-up
direction(-1, 1).  % Diagonal-right-up

% Check if the word exists starting at (X, Y) in direction (DX, DY)
check_word(Grid, Word, X, Y, DX, DY) :-
    string_chars(Word, Chars),
    check_word(Grid, Chars, X, Y, DX, DY, 0).

check_word(_, [], _, _, _, _, _).
check_word(Grid, [C|Chars], X, Y, DX, DY, Index) :-
    nth0(X, Grid, Row),
    nth0(Y, Row, C),
    NextX is X + DX,
    NextY is Y + DY,
    NextIndex is Index + 1,
    check_word(Grid, Chars, NextX, NextY, DX, DY, NextIndex).

% Count all occurrences of the word in the grid
count_word(Grid, Word, Count) :-
    findall(_, (
        nth0(X, Grid, _),
        nth0(Y, _, _),
        direction(DX, DY),
        check_word(Grid, Word, X, Y, DX, DY)
    ), Occurrences),
    length(Occurrences, Count).

% Main predicate to solve the word search
word_search(FilePath, Word, Count) :-
    read_grid(FilePath, Grid),
    count_word(Grid, Word, Count).
