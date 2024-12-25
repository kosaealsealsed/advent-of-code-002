-module(xmas_counter).
-export([main/0, read_grid/1, count_xmas/1, count_all_xmas_patterns/1, check_word/6]).

%% Read the grid from the input file
read_grid(FilePath) ->
    {ok, Binary} = file:read_file(FilePath),
    Lines = binary:split(Binary, <<"\n">>, [global, trim]),
    [binary_to_list(Line) || Line <- Lines].

%% Check if a word exists in a given direction
check_word(Grid, X, Y, Dx, Dy, Word) ->
    Len = length(Word),
    Rows = length(Grid),
    Cols = length(hd(Grid)),
    check_word(Grid, X, Y, Dx, Dy, Word, Len, Rows, Cols, 0).

check_word(_Grid, _X, _Y, _Dx, _Dy, _Word, Len, _Rows, _Cols, Len) ->
    true;
check_word(Grid, X, Y, Dx, Dy, Word, Len, Rows, Cols, I) ->
    Nx = X + I * Dx,
    Ny = Y + I * Dy,
    if
        Nx < 0; Ny < 0; Nx >= Rows; Ny >= Cols ->
            false;
        true ->
            GridRow = lists:nth(Nx + 1, Grid),
            GridChar = lists:nth(Ny + 1, GridRow),
            WordChar = lists:nth(I + 1, Word),
            case GridChar =/= WordChar of
                true -> false;
                false -> check_word(Grid, X, Y, Dx, Dy, Word, Len, Rows, Cols, I + 1)
            end
    end.

%% Count occurrences of the word "XMAS" in all directions
count_xmas(Grid) ->
    TargetWord = "XMAS",
    Directions = [{0, 1}, {1, 0}, {1, 1}, {1, -1}, {0, -1}, {-1, 0}, {-1, -1}, {-1, 1}],
    Rows = length(Grid),
    Cols = length(hd(Grid)),
    lists:sum([1 || R <- lists:seq(0, Rows - 1),
                   C <- lists:seq(0, Cols - 1),
                   {Dx, Dy} <- Directions,
                   check_word(Grid, R, C, Dx, Dy, TargetWord)]).

%% Count all X-MAS patterns in the grid
count_all_xmas_patterns(Grid) ->
    Rows = length(Grid),
    Cols = length(hd(Grid)),
    lists:sum([
        1 || R <- lists:seq(1, Rows - 2),
             C <- lists:seq(1, Cols - 2),
             lists:nth(C + 1, lists:nth(R + 1, Grid)) =:= $A,
             (
                 lists:nth(C, lists:nth(R, Grid)) =:= $M andalso
                 lists:nth(C + 2, lists:nth(R, Grid)) =:= $S andalso
                 lists:nth(C, lists:nth(R + 2, Grid)) =:= $M andalso
                 lists:nth(C + 2, lists:nth(R + 2, Grid)) =:= $S
             ) orelse
             (
                 lists:nth(C, lists:nth(R, Grid)) =:= $S andalso
                 lists:nth(C + 2, lists:nth(R, Grid)) =:= $M andalso
                 lists:nth(C, lists:nth(R + 2, Grid)) =:= $S andalso
                 lists:nth(C + 2, lists:nth(R + 2, Grid)) =:= $M
             ) orelse
             (
                 lists:nth(C, lists:nth(R, Grid)) =:= $M andalso
                 lists:nth(C + 2, lists:nth(R, Grid)) =:= $M andalso
                 lists:nth(C, lists:nth(R + 2, Grid)) =:= $S andalso
                 lists:nth(C + 2, lists:nth(R + 2, Grid)) =:= $S
             ) orelse
             (
                 lists:nth(C, lists:nth(R, Grid)) =:= $S andalso
                 lists:nth(C + 2, lists:nth(R, Grid)) =:= $S andalso
                 lists:nth(C, lists:nth(R + 2, Grid)) =:= $M andalso
                 lists:nth(C + 2, lists:nth(R + 2, Grid)) =:= $M
             )
    ]).



%% Main function
main() ->
    FilePath = "input.txt",
    Grid = read_grid(FilePath),
    case Grid of
        [] ->
            io:format("Error: Grid is empty or invalid.~n");
        _ ->
            XmasCount = count_xmas(Grid),
            io:format("Count of XMAS: ~p~n", [XmasCount]),
            XmasPatterns = count_all_xmas_patterns(Grid),
            io:format("Total X-MAS patterns: ~p~n", [XmasPatterns])
    end.
