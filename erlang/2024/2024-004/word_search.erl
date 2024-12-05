-module(word_search).
-export([main/0]).

% Directions: {dx, dy}
-define(DIRECTIONS, [
    {0, 1},    % Right
    {1, 0},    % Down
    {1, 1},    % Diagonal-right-down
    {1, -1},   % Diagonal-left-down
    {0, -1},   % Left
    {-1, 0},   % Up
    {-1, -1},  % Diagonal-left-up
    {-1, 1}    % Diagonal-right-up
]).
-define(TARGET_WORD, "XMAS").

% Reads the grid from a file and converts it into a list of lists of strings
read_grid(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    Lines = string:split(Content, "\n", all),
    [string:split(Line, "", all) || Line <- Lines].

% Checks if the word exists in the grid starting from {X, Y} in the direction {Dx, Dy}
check_word(Grid, Word, X, Y, Dx, Dy) ->
    WordLength = length(Word),
    lists:all(
        fun(I) ->
            Nx = X + I * Dx,
            Ny = Y + I * Dy,
            case (Nx < 0 orelse Ny < 0 orelse Nx >= length(Grid) orelse Ny >= length(lists:nth(1, Grid))) of
                true -> false;
                false -> 
                    Row = lists:nth(Nx + 1, Grid),
                    Char = lists:nth(Ny + 1, Row),
                    Char == lists:nth(I + 1, Word)
            end
        end,
        lists:seq(0, WordLength - 1)
    ).

% Counts occurrences of the target word in the grid
count_occurrences(Grid, Word) ->
    lists:foldl(
        fun({X, Y, {Dx, Dy}}, Acc) ->
            case check_word(Grid, Word, X, Y, Dx, Dy) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        [{X, Y, Dir} || X <- lists:seq(0, length(Grid) - 1), 
                        Y <- lists:seq(0, length(lists:nth(1, Grid)) - 1), 
                        Dir <- ?DIRECTIONS]
    ).


% Counts XMAS patterns in the grid
count_xmas_patterns(Grid) ->
    GridHeight = length(Grid),
    GridWidth = case Grid of
        [] -> 0;
        _ -> length(lists:nth(1, Grid))
    end,
    case (GridHeight >= 3 andalso GridWidth >= 3) of
        false -> 0; % Return 0 if the grid is too small
        true ->
            lists:foldl(
                fun({X, Y}, Acc) ->
                    Center = lists:nth(Y + 1, lists:nth(X + 1, Grid)),
                    TopLeft = lists:nth(Y, lists:nth(X, Grid)),
                    TopRight = lists:nth(Y + 2, lists:nth(X, Grid)),
                    BottomLeft = lists:nth(Y, lists:nth(X + 2, Grid)),
                    BottomRight = lists:nth(Y + 2, lists:nth(X + 2, Grid)),
                    if
                        Center == "A" andalso
                        ((TopLeft == "M" andalso TopRight == "S" andalso BottomLeft == "M" andalso BottomRight == "S") orelse
                         (TopLeft == "S" andalso TopRight == "M" andalso BottomLeft == "S" andalso BottomRight == "M") orelse
                         (TopLeft == "M" andalso TopRight == "M" andalso BottomLeft == "S" andalso BottomRight == "S") orelse
                         (TopLeft == "S" andalso TopRight == "S" andalso BottomLeft == "M" andalso BottomRight == "M")) ->
                            Acc + 1;
                        true -> Acc
                    end
                end,
                0,
                [{X, Y} || X <- lists:seq(1, GridHeight - 2), 
                           Y <- lists:seq(1, GridWidth - 2)]
            )
    end.


% Main function to read the file and calculate the counts
%main() ->
%    FilePath = "input.txt",
%    Grid = read_grid(FilePath),

%    XmasCount = count_occurrences(Grid, ?TARGET_WORD),
%    io:format("Count of XMAS occurrences: ~p~n", [XmasCount]),

%   PatternCount = count_xmas_patterns(Grid),
%    io:format("Total XMAS patterns: ~p~n", [PatternCount]).

main() ->
    FilePath = "./input.txt",  % Adjust the path to match your file location
    Grid = read_grid(FilePath),
    XmasCount = count_occurrences(Grid, ?TARGET_WORD),
    io:format("Count of XMAS occurrences: ~p~n", [XmasCount]),
    PatternCount = count_xmas_patterns(Grid),
    io:format("Total XMAS patterns: ~p~n", [PatternCount]).
