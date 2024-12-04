# Load the input file
file_path <- "input.txt"
grid <- readLines(file_path)

# Define the target word and grid dimensions
target_word <- "XMAS"
rows <- length(grid)
cols <- nchar(grid[1])

# Define the directions for moving in the grid
directions <- list(
  c(0, 1),    # Right
  c(1, 0),    # Down
  c(1, 1),    # Diagonal-right-down
  c(1, -1),   # Diagonal-left-down
  c(0, -1),   # Left
  c(-1, 0),   # Up
  c(-1, -1),  # Diagonal-left-up
  c(-1, 1)    # Diagonal-right-up
)

# Convert the grid to a list of characters
grid_matrix <- do.call(rbind, strsplit(grid, ""))

# Function to check if a word exists in a given direction
check_word <- function(x, y, dx, dy) {
  for (i in 0:(nchar(target_word) - 1)) {
    nx <- x + i * dx
    ny <- y + i * dy
    if (nx < 1 || ny < 1 || nx > rows || ny > cols || grid_matrix[nx, ny] != substr(target_word, i + 1, i + 1)) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Count all occurrences of the target word
count <- 0
for (r in 1:rows) {
  for (c in 1:cols) {
    for (direction in directions) {
      dx <- direction[1]
      dy <- direction[2]
      if (check_word(r, c, dx, dy)) {
        count <- count + 1
      }
    }
  }
}

cat("count is", count, "\n")

# Function to count all X-MAS patterns
count_all_xmas_patterns <- function() {
  count <- 0
  # Traverse the grid, ensuring bounds for a 3x3 X-MAS pattern
  for (r in 2:(rows - 1)) {
    for (c in 2:(cols - 1)) {
      center <- grid_matrix[r, c]
      top_left <- grid_matrix[r - 1, c - 1]
      top_right <- grid_matrix[r - 1, c + 1]
      bottom_left <- grid_matrix[r + 1, c - 1]
      bottom_right <- grid_matrix[r + 1, c + 1]
      
      # Check all valid X-MAS configurations
      if (center == "A") {
        # Pattern 1: M.S
        if (top_left == "M" && top_right == "S" && bottom_left == "M" && bottom_right == "S") {
          count <- count + 1
        }
        # Pattern 2: S.M
        if (top_left == "S" && top_right == "M" && bottom_left == "S" && bottom_right == "M") {
          count <- count + 1
        }
        # Pattern 3: M.M
        if (top_left == "M" && top_right == "M" && bottom_left == "S" && bottom_right == "S") {
          count <- count + 1
        }
        # Pattern 4: S.S
        if (top_left == "S" && top_right == "S" && bottom_left == "M" && bottom_right == "M") {
          count <- count + 1
        }
      }
    }
  }
  return(count)
}

# Count the X-MAS patterns in the grid
total_xmas_patterns <- count_all_xmas_patterns()
cat("total xmas patterns is", total_xmas_patterns, "\n")

#Both solutions
cat("count is", count, "\n")
cat("total xmas patterns is", total_xmas_patterns, "\n")
