% Define the file path and target word
file_path = "input.txt";
target_word = "XMAS";

% Read the file and store the grid
fid = fopen(file_path, 'r');
grid = {};
while ~feof(fid)
    grid{end+1} = fgetl(fid); % Read each line as a string
end
fclose(fid);

% Convert grid to character matrix for easier indexing
grid = char(grid);

% Define grid dimensions
[rows, cols] = size(grid);

% Directions for moving in the grid (right, down, diagonal-right-down, diagonal-left-down)
directions = [
    0,  1;   % Right
    1,  0;   % Down
    1,  1;   % Diagonal-right-down
    1, -1;   % Diagonal-left-down
    0, -1;   % Left
    -1, 0;   % Up
    -1, -1;  % Diagonal-left-up
    -1,  1   % Diagonal-right-up
];

% Function to check if a word exists in a given direction
function exists = check_word(grid, target_word, x, y, dx, dy, rows, cols)
    exists = true;
    len = length(target_word);
    for i = 0:(len-1)
        nx = x + i * dx;
        ny = y + i * dy;
        if nx < 1 || ny < 1 || nx > rows || ny > cols || grid(nx, ny) ~= target_word(i+1)
            exists = false;
            return;
        end
    end
end

% Count all occurrences of the target word
count = 0;
for r = 1:rows
    for c = 1:cols
        for d = 1:size(directions, 1)
            dx = directions(d, 1);
            dy = directions(d, 2);
            if check_word(grid, target_word, r, c, dx, dy, rows, cols)
                count += 1;
            end
        end
    end
end

fprintf("count is %d.\n", count);

% Function to count all X-MAS patterns
function count = count_all_xmas_patterns(grid, rows, cols)
    count = 0;
    for r = 2:(rows-1)
        for c = 2:(cols-1)
            center = grid(r, c);
            top_left = grid(r-1, c-1);
            top_right = grid(r-1, c+1);
            bottom_left = grid(r+1, c-1);
            bottom_right = grid(r+1, c+1);

            % Check all valid X-MAS configurations
            if center == 'A'
                % Pattern 1: M.S
                if top_left == 'M' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'S'
                    count += 1;
                % Pattern 2: S.M
                elseif top_left == 'S' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'M'
                    count += 1;
                % Pattern 3: M.M
                elseif top_left == 'M' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'S'
                    count += 1;
                % Pattern 4: S.S
                elseif top_left == 'S' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'M'
                    count += 1;
                end
            end
        end
    end
end

% Count the X-MAS patterns in the grid
total_xmas_patterns = count_all_xmas_patterns(grid, rows, cols);
fprintf("total xmas patterns is %d.\n", total_xmas_patterns);
