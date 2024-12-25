// Define the file path and target word
def filePath = "/uploads/input.txt"
def targetWord = "XMAS"

// Read the file and store the grid
def grid = []
new File(filePath).eachLine { line ->
    grid << line.trim()
}

// Define grid dimensions
def rows = grid.size()
def cols = grid[0].size()

// Define the directions
def directions = [
    [0, 1],    // Right
    [1, 0],    // Down
    [1, 1],    // Diagonal-right-down
    [1, -1],   // Diagonal-left-down
    [0, -1],   // Left
    [-1, 0],   // Up
    [-1, -1],  // Diagonal-left-up
    [-1, 1]    // Diagonal-right-up
]

// Function to check if a word exists in a given direction
def checkWord(grid, rows, cols, targetWord, x, y, dx, dy) {
    for (int i = 0; i < targetWord.size(); i++) {
        def nx = x + i * dx
        def ny = y + i * dy
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != targetWord[i]) {
            return false
        }
    }
    return true
}

// Count all occurrences of the target word
def countOccurrences = 0
for (int r = 0; r < rows; r++) {
    for (int c = 0; c < cols; c++) {
        for (def dir in directions) {
            if (checkWord(grid, rows, cols, targetWord, r, c, dir[0], dir[1])) {
                countOccurrences++
            }
        }
    }
}

println "Count of occurrences of '${targetWord}' is ${countOccurrences}."

// Function to count X-MAS patterns
def countXmasPatterns(grid, rows, cols) {
    def count = 0
    for (int r = 1; r < rows - 1; r++) {
        for (int c = 1; c < cols - 1; c++) {
            def center = grid[r][c]
            def topLeft = grid[r - 1][c - 1]
            def topRight = grid[r - 1][c + 1]
            def bottomLeft = grid[r + 1][c - 1]
            def bottomRight = grid[r + 1][c + 1]

            // Check valid X-MAS configurations
            if (center == 'A') {
                if (topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S') count++
                if (topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M') count++
                if (topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S') count++
                if (topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M') count++
            }
        }
    }
    return count
}

// Count the X-MAS patterns
def totalXmasPatterns = countXmasPatterns(grid, rows, cols)
println "Total X-MAS patterns is ${totalXmasPatterns}."
