-module(xmas_grid).
-export([main/0]).

% Define the target word and its length
target_word() -> "XMAS".
word_length() -> length(target_word()).

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

% Check if a word exists in a given direction
check_word(Grid, {X, Y}, {DX, DY}) ->
    WordLength = word_length(),
    GridSize = length(Grid),
    Positions = lists:seq(0, WordLength - 1),
    ValidChars = lists:foldl(
        fun(I, Acc) ->
            NewX = X + I * DX,
            NewY = Y + I * DY,
            case is_valid(Grid, NewX, NewY) of
                true -> [element(NewX + 1, element(NewY + 1, Grid)) | Acc];
                false -> Acc
            end
        end, [], Positions),
    ValidChars == target_word().

% Check if a position is valid in the grid
is_valid(Grid, R, C) ->
    R >= 0, 
    C >= 0, 
    R < length(Grid), 
    C < length(element(1, Grid)).

% Count occurrences of the target word
count_occurrences(Grid) ->
    GridSize = length(Grid),
    lists:foldl(
        fun(R, Acc) ->
            lists:foldl(
                fun(C, InnerAcc) ->
                    lists:foldl(
                        fun(Dir, DirAcc) ->
                            case check_word(Grid, {R, C}, Dir) of
                                true -> DirAcc + 1;
                                false -> DirAcc
                            end
                        end, InnerAcc, directions())
                end, Acc, lists:seq(0, length(element(1, Grid)) - 1))
        end, 0, lists:seq(0, GridSize - 1)).

% Count all X-MAS patterns
count_xmas_patterns(Grid) ->
    GridSize = length(Grid),
    lists:foldl(
        fun(R, Acc) ->
            lists:foldl(
                fun(C, InnerAcc) ->
                    Center = element(R + 1, element(C + 1, Grid)),
                    TopLeft = element(R, element(C, Grid)),
                    TopRight = element(R, element(C + 2, Grid)),
                    BottomLeft = element(R + 2, element(C, Grid)),
                    BottomRight = element(R + 2, element(C + 2, Grid)),
                    case {Center, TopLeft, TopRight, BottomLeft, BottomRight} of
                        {'A', 'M', 'S', 'M', 'S'} -> InnerAcc + 1;
                        {'A', 'S', 'M', 'S', 'M'} -> InnerAcc + 1;
                        {'A', 'M', 'M', 'S', 'S'} -> InnerAcc + 1;
                        {'A', 'S', 'S', 'M', 'M'} -> InnerAcc + 1;
                        _ -> InnerAcc
                    end
                end, Acc, lists:seq(1, length(element(1, Grid)) - 2))
        end, 0, lists:seq(1, GridSize - 2)).

% Main function to process the input file and compute results
main() ->
    {ok, Contents} = file:read_file("input.txt"),
    Grid = string:split(Contents, "\n", all),
    WordCount = count_occurrences(Grid),
    io:format("Count is ~p~n", [WordCount]),
    XmasPatternCount = count_xmas_patterns(Grid),
    io:format("Total XMAS patterns is ~p~n", [XmasPatternCount]).
