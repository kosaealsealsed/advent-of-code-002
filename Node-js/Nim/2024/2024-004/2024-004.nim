import os

# Define the file path and target word
let filePath = "/uploads/input.txt"
let targetWord = "XMAS"

# Read the file and store the grid
var grid: seq[string]
for line in lines(open(filePath)):
    grid.add(line)

# Define grid dimensions
let rows = grid.len
let cols = grid[0].len

# Define directions for moving in the grid
let directions = [
    (0, 1),    # Right
    (1, 0),    # Down
    (1, 1),    # Diagonal-right-down
    (1, -1),   # Diagonal-left-down
    (0, -1),   # Left
    (-1, 0),   # Up
    (-1, -1),  # Diagonal-left-up
    (-1, 1)    # Diagonal-right-up
]

# Function to check if a word exists in a given direction
proc checkWord(x, y, dx, dy: int): bool =
    for i in 0..<targetWord.len:
        let nx = x + i * dx
        let ny = y + i * dy
        if nx < 0 or ny < 0 or nx >= rows or ny >= cols or grid[nx][ny] != targetWord[i]:
            return false
    return true

# Count all occurrences of the target word
var count = 0
for r in 0..<rows:
    for c in 0..<cols:
        for direction in directions:
            if checkWord(r, c, direction[0], direction[1]):
                inc(count)

echo count

# Function to count all X-MAS patterns
proc countAllXmasPatterns(): int =
    var count = 0
    # Traverse the grid, ensuring bounds for a 3x3 X-MAS pattern
    for r in 1..<rows-1:
        for c in 1..<cols-1:
            let center = grid[r][c]
            let topLeft = grid[r-1][c-1]
            let topRight = grid[r-1][c+1]
            let bottomLeft = grid[r+1][c-1]
            let bottomRight = grid[r+1][c+1]

            # Check all valid X-MAS configurations
            if center == 'A':
                # Pattern 1: M.S
                if topLeft == 'M' and topRight == 'S' and bottomLeft == 'M' and bottomRight == 'S':
                    inc(count)
                # Pattern 2: S.M
                elif topLeft == 'S' and topRight == 'M' and bottomLeft == 'S' and bottomRight == 'M':
                    inc(count)
                # Pattern 3: M.M
                elif topLeft == 'M' and topRight == 'M' and bottomLeft == 'S' and bottomRight == 'S':
                    inc(count)
                # Pattern 4: S.S
                elif topLeft == 'S' and topRight == 'S' and bottomLeft == 'M' and bottomRight == 'M':
                    inc(count)

    return count

# Count the X-MAS patterns in the grid
let totalXmasPatterns = countAllXmasPatterns()
echo totalXmasPatterns
