\\ Define the file path and the target word
file_path = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\pari-gp\\2024\\2024-004\\input.txt"
target_word = "XMAS"

\\ Read the file content into a grid
read_grid(file_path) = {
    lines = readstr(file_path);
    return(Vec(split(lines, "\n"))); \\ Split the lines into a vector
}

\\ Check if a word exists in a given direction
check_word(grid, target_word, x, y, dx, dy) = {
    rows = #grid;
    cols = #grid[1];
    len_word = #target_word;
    for (i = 0, len_word - 1,
        nx = x + i * dx;
        ny = y + i * dy;
        if (nx < 1 || ny < 1 || nx > rows || ny > cols || grid[nx][ny] != target_word[i + 1],
            return(0)
        )
    );
    return(1)
}

\\ Count all occurrences of the target word in the grid
count_occurrences(grid, target_word) = {
    directions = [
        [0, 1],   \\ Right
        [1, 0],   \\ Down
        [1, 1],   \\ Diagonal-right-down
        [1, -1],  \\ Diagonal-left-down
        [0, -1],  \\ Left
        [-1, 0],  \\ Up
        [-1, -1], \\ Diagonal-left-up
        [-1, 1]   \\ Diagonal-right-up
    ];
    rows = #grid;
    cols = #grid[1];
    count = 0;
    for (r = 1, rows,
        for (c = 1, cols,
            for (d = 1, #directions,
                dx = directions[d][1];
                dy = directions[d][2];
                count += check_word(grid, target_word, r, c, dx, dy);
            )
        )
    );
    return(count)
}

\\ Main execution
grid = read_grid(file_path);
occurrences = count_occurrences(grid, target_word);
print("The word '", target_word, "' appears ", occurrences, " times in the grid.");
