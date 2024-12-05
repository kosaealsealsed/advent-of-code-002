# Function to read the grid from the input file
def read_grid(file_path : String) : Array(Array(Char))
  File.read_lines(file_path).map { |line| line.chars }
end

# Define the target word and movement directions
TARGET_WORD = "XMAS"
DIRECTIONS = [
  {0, 1},    # Right
  {1, 0},    # Down
  {1, 1},    # Diagonal-right-down
  {1, -1},   # Diagonal-left-down
  {0, -1},   # Left
  {-1, 0},   # Up
  {-1, -1},  # Diagonal-left-up
  {-1, 1}    # Diagonal-right-up
]

# Function to check if the word exists in a given direction
def check_word(grid : Array(Array(Char)), x : Int32, y : Int32, dx : Int32, dy : Int32) : Bool
  rows = grid.size
  cols = grid[0].size
  (0...TARGET_WORD.size).each do |i|
    nx = x + i * dx
    ny = y + i * dy
    return false if nx < 0 || ny < 0 || nx >= rows || ny >= cols
    return false if grid[nx][ny] != TARGET_WORD[i]
  end
  true
end

# Function to count all occurrences of the target word
def count_occurrences(grid : Array(Array(Char))) : Int32
  count = 0
  rows = grid.size
  cols = grid[0].size
  (0...rows).each do |x|
    (0...cols).each do |y|
      DIRECTIONS.each do |dx, dy|
        count += 1 if check_word(grid, x, y, dx, dy)
      end
    end
  end
  count
end

# Function to count all X-MAS patterns
def count_xmas_patterns(grid : Array(Array(Char))) : Int32
  count = 0
  rows = grid.size
  cols = grid[0].size
  (1...(rows - 1)).each do |x|
    (1...(cols - 1)).each do |y|
      center = grid[x][y]
      top_left = grid[x - 1][y - 1]
      top_right = grid[x - 1][y + 1]
      bottom_left = grid[x + 1][y - 1]
      bottom_right = grid[x + 1][y + 1]

      if center == 'A' &&
         ((top_left == 'M' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'S') ||
          (top_left == 'S' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'M') ||
          (top_left == 'M' && top_right == 'M' && bottom_left == 'S' && bottom_right == 'S') ||
          (top_left == 'S' && top_right == 'S' && bottom_left == 'M' && bottom_right == 'M'))
        count += 1
      end
    end
  end
  count
end

# Main function
def main
  file_path = "/uploads/input.txt"
  grid = read_grid(file_path)
  puts "Count of XMAS occurrences: #{count_occurrences(grid)}"
  puts "Total XMAS patterns: #{count_xmas_patterns(grid)}"
end

main
