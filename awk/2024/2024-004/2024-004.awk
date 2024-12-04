BEGIN {
    # Define target word and its length
    target_word = "XMAS"
    target_len = length(target_word)

    # Define movement directions
    directions[1] = "0 1"    # Right
    directions[2] = "1 0"    # Down
    directions[3] = "1 1"    # Diagonal-right-down
    directions[4] = "1 -1"   # Diagonal-left-down
    directions[5] = "0 -1"   # Left
    directions[6] = "-1 0"   # Up
    directions[7] = "-1 -1"  # Diagonal-left-up
    directions[8] = "-1 1"   # Diagonal-right-up
}

# Read input grid
{
    grid[NR] = $0
    row_length = length($0)
    grid_rows = NR
}

# Function to check if the word exists in a direction
function check_word(x, y, dx, dy,   i, nx, ny, char, word_char) {
    for (i = 0; i < target_len; i++) {
        nx = x + i * dx
        ny = y + i * dy

        # Bounds check
        if (nx < 1 || ny < 1 || nx > grid_rows || ny > row_length) {
            return 0
        }

        # Character match check
        char = substr(grid[nx], ny, 1)
        word_char = substr(target_word, i + 1, 1)
        if (char != word_char) {
            return 0
        }
    }
    return 1
}

# Function to count X-MAS patterns
function count_xmas_patterns(   count, r, c, center, top_left, top_right, bottom_left, bottom_right) {
    count = 0
    for (r = 2; r <= grid_rows - 1; r++) {
        for (c = 2; c <= row_length - 1; c++) {
            center = substr(grid[r], c, 1)
            top_left = substr(grid[r - 1], c - 1, 1)
            top_right = substr(grid[r - 1], c + 1, 1)
            bottom_left = substr(grid[r + 1], c - 1, 1)
            bottom_right = substr(grid[r + 1], c + 1, 1)

            if (center == "A") {
                if ((top_left == "M" && top_right == "S" && bottom_left == "M" && bottom_right == "S") ||
                    (top_left == "S" && top_right == "M" && bottom_left == "S" && bottom_right == "M") ||
                    (top_left == "M" && top_right == "M" && bottom_left == "S" && bottom_right == "S") ||
                    (top_left == "S" && top_right == "S" && bottom_left == "M" && bottom_right == "M")) {
                    count++
                }
            }
        }
    }
    return count
}

END {
    # Count occurrences of "XMAS"
    word_count = 0
    for (r = 1; r <= grid_rows; r++) {
        for (c = 1; c <= row_length; c++) {
            for (d = 1; d <= 8; d++) {
                split(directions[d], dir, " ")
                dx = dir[1]
                dy = dir[2]
                if (check_word(r, c, dx, dy)) {
                    word_count++
                }
            }
        }
    }

    # Print word occurrences
    print "count is " word_count

    # Count X-MAS patterns
    xmas_pattern_count = count_xmas_patterns()
    print "total xmas patterns is " xmas_pattern_count
}
