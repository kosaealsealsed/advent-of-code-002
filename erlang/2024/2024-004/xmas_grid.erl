-module(xmas_grid).

-export([main/0, count_occurrences/1, count_xmas_patterns/1]).

% Define the target word and its length
-define(TARGET_WORD, "XMAS").
-define(WORD_LENGTH, length(?TARGET_WORD)).

% Directions for moving in the grid
directions() ->
    [{0, 1},   % Right
     {1, 0},   % Down
     {1, 1},   % Diagonal-right-down
     {1, -1},  % Diagonal-left-down
     {0, -1},  % Left
     {-1, 0},  % Up
     {-1, -1}, % Diagonal-left-up
     {-1, 1}]. % Diagonal-right-up

% Function to check if a word exists in a given direction
check_word(Grid, {X, Y}, {Dx, Dy}) ->
    WordLength = ?WORD_LENGTH,
    Positions = lists:seq(0, WordLength - 1),
    ValidPositions = lists:filter(fun({R, C}) -> 
        R >= 0, C >= 0, 
        R < length(Grid), 
        C < length(lists:nth(1, Grid)) 
    end, 
    lists:map(fun(I) -> {X + I * Dx, Y + I * Dy} end, Positions)),
    Chars = lists:map(fun({R, C}) -> lists:nth(C + 1, lists:nth(R + 1, Grid)) end, ValidPositions),
    length(Chars) =:= WordLength andalso Chars =:= string:chars(?TARGET_WORD).

% Count all occurrences of the target word
count_occurrences(Grid) ->
    length([true || 
        R <- lists:seq(0, length(Grid) - 1), 
        C <- lists:seq(0, length(lists:nth(1, Grid)) - 1), 
        Dir <- directions(), 
        check_word(Grid, {R, C}, Dir)]).

% Function to count all X-MAS patterns
count_xmas_patterns(Grid) ->
    length([true || 
        R <- lists:seq(1, length(Grid) - 2), 
        C <- lists:seq(1, length(lists:nth(1, Grid)) - 2),
        Center = lists:nth(C + 1, lists:nth(R + 1, Grid)),
        TopLeft = lists:nth(C, lists:nth(R, Grid)),
        TopRight = lists:nth(C + 2, lists:nth(R, Grid)),
        BottomLeft = lists:nth(C, lists:nth(R + 2, Grid)),
        BottomRight = lists:nth(C + 2, lists:nth(R + 2, Grid)),
        Center =:= $A,
        lists:member({TopLeft, TopRight, BottomLeft, BottomRight}, [
            {$M, $S, $M, $S}, 
            {$S, $M, $S, $M}, 
            {$M, $M, $S, $S}, 
            {$S, $S, $M, $M}
        ])]).

% Main function to process the input file and compute results
main() ->
    {ok, Contents} = file:read_file("input.txt"),
    Grid = string:split(binary_to_list(Contents), "\n", all),
    WordCount = count_occurrences(Grid),
    io:format("count is ~p~n", [WordCount]),
    XmasPatternCount = count_xmas_patterns(Grid),
    io:format("total xmas patterns is ~p~n", [XmasPatternCount]).
