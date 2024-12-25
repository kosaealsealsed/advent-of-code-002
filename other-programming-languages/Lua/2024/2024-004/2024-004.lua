-- Read the grid from a file
function read_grid(file_path)
    local grid = {}
    for line in io.lines(file_path) do
        table.insert(grid, line)
    end
    return grid
end

-- Get the dimensions of the grid
function grid_dimensions(grid)
    local rows = #grid
    local cols = #grid[1]
    return rows, cols
end

-- Check if a word exists in a specific direction
function check_word(grid, target_word, x, y, dx, dy, rows, cols)
    for i = 1, #target_word do
        local nx = x + (i - 1) * dx
        local ny = y + (i - 1) * dy
        if nx < 1 or ny < 1 or nx > rows or ny > cols or grid[nx]:sub(ny, ny) ~= target_word:sub(i, i) then
            return false
        end
    end
    return true
end

-- Count all occurrences of the target word
function count_word_occurrences(grid, target_word)
    local rows, cols = grid_dimensions(grid)
    local directions = {{0, 1}, {1, 0}, {1, 1}, {1, -1}, {0, -1}, {-1, 0}, {-1, -1}, {-1, 1}}
    local count = 0

    for r = 1, rows do
        for c = 1, cols do
            for _, dir in ipairs(directions) do
                local dx, dy = dir[1], dir[2]
                if check_word(grid, target_word, r, c, dx, dy, rows, cols) then
                    count = count + 1
                end
            end
        end
    end
    return count
end

-- Count all X-MAS patterns
function count_xmas_patterns(grid)
    local rows, cols = grid_dimensions(grid)
    local count = 0

    for r = 2, rows - 1 do
        for c = 2, cols - 1 do
            local center = grid[r]:sub(c, c)
            local top_left = grid[r - 1]:sub(c - 1, c - 1)
            local top_right = grid[r - 1]:sub(c + 1, c + 1)
            local bottom_left = grid[r + 1]:sub(c - 1, c - 1)
            local bottom_right = grid[r + 1]:sub(c + 1, c + 1)

            if center == "A" then
                -- Check all valid X-MAS configurations
                if top_left == "M" and top_right == "S" and bottom_left == "M" and bottom_right == "S" then
                    count = count + 1
                elseif top_left == "S" and top_right == "M" and bottom_left == "S" and bottom_right == "M" then
                    count = count + 1
                elseif top_left == "M" and top_right == "M" and bottom_left == "S" and bottom_right == "S" then
                    count = count + 1
                elseif top_left == "S" and top_right == "S" and bottom_left == "M" and bottom_right == "M" then
                    count = count + 1
                end
            end
        end
    end
    return count
end

-- Main function
function main()
    local file_path = "/uploads/input.txt"
    local target_word = "XMAS"

    -- Read the grid
    local grid = read_grid(file_path)

    -- Count occurrences of the target word
    local word_count = count_word_occurrences(grid, target_word)
    print("Count of '" .. target_word .. "': " .. word_count)

    -- Count X-MAS patterns
    local xmas_patterns = count_xmas_patterns(grid)
    print("Total X-MAS patterns: " .. xmas_patterns)
end

-- Run the main function
main()
