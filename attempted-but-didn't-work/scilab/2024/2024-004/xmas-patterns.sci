// Define the file path and target word
file_path = "input.txt";
target_word = "XMAS";

// Read the file and store the grid
file_handle = mopen(file_path, 'r');
grid = [];
while ~meof(file_handle) do
    line = mgetl(file_handle); // Read each line as a string
    grid = [grid; line];       // Append to the grid
end
mclose(file_handle);

// Convert grid to a character matrix for easier indexing
grid = ascii(grid); // Ensure it's treated as a character matrix

// Define grid dimensions
[rows, cols] = size(grid);

// Directions for moving in the grid (right, down, diagonal-right-down, diagonal-left-down)
directions = [
    0,  1;   // Right
    1,  0;   // Down
    1,  1;   // Diagonal-right-down
    1, -1;   // Diagonal-left-down
    0, -1;   // Left
    -1, 0;   // Up
    -1, -1;  // Diagonal-left-up
    -1,  1   // Diagonal-right-up
];

// Function to check if a word exists in a given direction
function exists = check_word(grid, target_word, x, y, dx, dy, rows, cols)
    exists = %t; // Initialize as true
    len = length(target_word);
    for i = 0:(len-1)
        nx = x + i * dx;
        ny = y + i * dy;

        // Check bounds
        if nx < 1 | ny < 1 | nx > rows | ny > cols then
            exists = %f; // Out of bounds
            return;
        end

        // Compare character
        if grid(nx, ny) <> ascii(target_word(i+1)) then
            exists = %f; // Character mismatch
            return;
        end
    end
endfunction


// Count all occurrences of the target word
count = 0;
for r = 1:rows
    for c = 1:cols
        for d = 1:size(directions, 1)
            dx = directions(d, 1);
            dy = directions(d, 2);
            if check_word(grid, target_word, r, c, dx, dy, rows, cols) then
                count = count + 1;
            end
        end
    end
end

disp("Count of " + target_word + " is " + string(count) + ".");

// Function to count all X-MAS patterns
function count = count_all_xmas_patterns(grid, rows, cols)
    count = 0;
    for r = 2:(rows-1)
        for c = 2:(cols-1)
            center = grid(r, c);
            top_left = grid(r-1, c-1);
            top_right = grid(r-1, c+1);
            bottom_left = grid(r+1, c-1);
            bottom_right = grid(r+1, c+1);

            // Check all valid X-MAS configurations
            if center == ascii('A') then
                // Pattern 1: M.S
                if top_left == ascii('M') & top_right == ascii('S') & bottom_left == ascii('M') & bottom_right == ascii('S') then
                    count = count + 1;
                // Pattern 2: S.M
                elseif top_left == ascii('S') & top_right == ascii('M') & bottom_left == ascii('S') & bottom_right == ascii('M') then
                    count = count + 1;
                // Pattern 3: M.M
                elseif top_left == ascii('M') & top_right == ascii('M') & bottom_left == ascii('S') & bottom_right == ascii('S') then
                    count = count + 1;
                // Pattern 4: S.S
                elseif top_left == ascii('S') & top_right == ascii('S') & bottom_left == ascii('M') & bottom_right == ascii('M') then
                    count = count + 1;
                end
            end
        end
    end
endfunction

// Count the X-MAS patterns in the grid
total_xmas_patterns = count_all_xmas_patterns(grid, rows, cols);
disp("Total XMAS patterns is " + string(total_xmas_patterns) + ".");
