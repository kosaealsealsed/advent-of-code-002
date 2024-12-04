# Function to read the grid from the input file
def read_grid(file_path)
  begin
    File.readlines(file_path).map(&:strip).reject(&:empty?)
  rescue StandardError => e
    puts "Error reading file: #{e.message}"
    []
  end
end

# Function to check if the word exists in a given direction
def check_word(grid, x, y, dx, dy, word, rows, cols)
  word.chars.each_with_index do |char, i|
    nx = x + i * dx
    ny = y + i * dy
    return false if nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != char
  end
  true
end

# Count occurrences of the word "XMAS" in all directions
def count_xmas(grid)
  target_word = "XMAS"
  directions = [
    [0, 1],   # Right
    [1, 0],   # Down
    [1, 1],   # Diagonal-right-down
    [1, -1],  # Diagonal-left-down
    [0, -1],  # Left
    [-1, 0],  # Up
    [-1, -1], # Diagonal-left-up
    [-1, 1]   # Diagonal-right-up
  ]

  rows = grid.length
  cols = grid[0].length
  count = 0

  (0...rows).each do |r|
    (0...cols).each do |c|
      directions.each do |dx, dy|
        count += 1 if check_word(grid, r, c, dx, dy, target_word, rows, cols)
      end
    end
  end

  count
end

# Count all X-MAS patterns
def count_all_xmas_patterns(grid)
  rows = grid.length
  cols = grid[0].length
  count = 0

  (1...(rows - 1)).each do |r|
    (1...(cols - 1)).each do |c|
      center = grid[r][c]
      top_left = grid[r - 1][c - 1]
      top_right = grid[r - 1][c + 1]
      bottom_left = grid[r + 1][c - 1]
      bottom_right = grid[r + 1][c + 1]

      if center == 'A'
        # Pattern 1: M.S
        count += 1 if top_left == 'M' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'S'
        # Pattern 2: S.M
        count += 1 if top_left == 'S' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'M'
        # Pattern 3: M.M
        count += 1 if top_left == 'M' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'S'
        # Pattern 4: S.S
        count += 1 if top_left == 'S' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'M'
      end
    end
  end

  count
end

# Main function
def main
  file_path = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt'
  grid = read_grid(file_path)

  if grid.empty?
    puts "Error: Grid is empty or invalid."
    return
  end

  xmas_count = count_xmas(grid)
  puts "Count of XMAS: #{xmas_count}"

  xmas_patterns = count_all_xmas_patterns(grid)
  puts "Total X-MAS patterns: #{xmas_patterns}"
end

# Run the main function
main
