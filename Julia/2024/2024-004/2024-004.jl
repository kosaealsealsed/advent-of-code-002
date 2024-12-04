# Reading the grid from the input file
function read_grid(file_path)
    return collect(eachline(file_path))  # Read each line as an element in an array
end

# Dimensions of the grid
function grid_dimensions(grid)
    rows = length(grid)
    cols = length(grid[1])
    return rows, cols
end

# Check if the target word exists in a specific direction
function check_word(grid, target_word, x, y, dx, dy)
    rows, cols = grid_dimensions(grid)
    for i in 1:length(target_word)
        nx, ny = x + (i - 1) * dx, y + (i - 1) * dy
        if nx < 1 || ny < 1 || nx > rows || ny > cols || grid[nx][ny] != target_word[i]
            return false
        end
    end
    return true
end

# Count all occurrences of the target word in the grid
function count_word_occurrences(grid, target_word)
    rows, cols = grid_dimensions(grid)
    directions = [(0, 1), (1, 0), (1, 1), (1, -1), (0, -1), (-1, 0), (-1, -1), (-1, 1)]
    count = 0
    for r in 1:rows
        for c in 1:cols
            for (dx, dy) in directions
                if check_word(grid, target_word, r, c, dx, dy)
                    count += 1
                end
            end
        end
    end
    return count
end

# Count all X-MAS patterns
function count_xmas_patterns(grid)
    rows, cols = grid_dimensions(grid)
    count = 0
    for r in 2:(rows - 1)
        for c in 2:(cols - 1)
            center = grid[r][c]
            top_left, top_right = grid[r - 1][c - 1], grid[r - 1][c + 1]
            bottom_left, bottom_right = grid[r + 1][c - 1], grid[r + 1][c + 1]

            if center == 'A'
                # Check all valid X-MAS configurations
                if top_left == 'M' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'S'
                    count += 1
                elseif top_left == 'S' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'M'
                    count += 1
                elseif top_left == 'M' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'S'
                    count += 1
                elseif top_left == 'S' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'M'
                    count += 1
                end
            end
        end
    end
    return count
end

# Main program
function main()
    file_path = "/uploads/input.txt"
    target_word = "XMAS"

    # Read and process the grid
    grid = read_grid(file_path)

    # Count occurrences of the target word
    word_count = count_word_occurrences(grid, target_word)
    println("Count of '$target_word': $word_count")

    # Count X-MAS patterns
    xmas_patterns = count_xmas_patterns(grid)
    println("Total X-MAS patterns: $xmas_patterns")
end

# Run the main function
main()
