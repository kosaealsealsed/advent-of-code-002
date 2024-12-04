// Define the input file path and target word
file_path = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt";
target_word = "XMAS";

// Read the file and store the grid as a matrix of characters
file = mopen(file_path, "r");
grid = [];
while ~meof(file)
    line = mgetl(file);
    grid = [grid; line];
end
mclose(file);

// Define the grid dimensions
[rows, cols] = size(grid);

// Define directions for moving in the grid
directions = [
    0,  1;  // Right
    1,  0;  // Down
    1,  1;  // Diagonal-right-down
    1, -1;  // Diagonal-left-down
    0, -1;  // Left
   -1,  0;  // Up
   -1, -1;  // Diagonal-left-up
   -1,  1   // Diagonal-right-up
];

// Function to check if a word exists in a given direction
function result = check_word(x, y, dx, dy, target_word, grid, rows, cols)
    len_word = length(target_word);
    for i = 0:len_word-1
        nx = x + i * dx;
        ny = y + i * dy;
        if nx < 1 | ny < 1 | nx > rows | ny > cols | grid(nx, ny) <> target_word(i + 1)
            result = %F;
            return;
        end
    end
    result = %T;
endfunction

// Count all occurrences of the target word
count = 0;
for r = 1:rows
    for c = 1:cols
        for d = 1:size(directions, 1)
            dx = directions(d, 1);
            dy = directions(d, 2);
            if check_word(r, c, dx, dy, target_word, grid, rows, cols)
                count = count + 1;
            end
        end
    end
end

// Define the output file path
output_file_path = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\scilab\\2024\\2024-004\\output-002.txt";

// Open the file for writing
file = mopen(output_file_path, "w");

// Write the result to the file
mfprintf(file, "Number of occurrences: %d\n", count);

// Close the file
mclose(file);

// Confirm the result has been written
disp("Result written to: " + output_file_path);

