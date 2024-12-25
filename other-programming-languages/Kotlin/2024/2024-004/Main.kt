import java.io.File

// Function to read the grid from the input file
fun readGrid(filePath: String): List<String> {
    return File(filePath).readLines().map { it.trim() }.filter { it.isNotEmpty() }
}

// Function to check if the word exists in a given direction
fun checkWord(
    grid: List<String>,
    x: Int,
    y: Int,
    dx: Int,
    dy: Int,
    word: String,
    rows: Int,
    cols: Int
): Boolean {
    for (i in word.indices) {
        val nx = x + i * dx
        val ny = y + i * dy
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != word[i]) {
            return false
        }
    }
    return true
}

// Count occurrences of the word "XMAS" in all directions
fun countXMAS(grid: List<String>): Int {
    val targetWord = "XMAS"
    val directions = listOf(
        Pair(0, 1),   // Right
        Pair(1, 0),   // Down
        Pair(1, 1),   // Diagonal-right-down
        Pair(1, -1),  // Diagonal-left-down
        Pair(0, -1),  // Left
        Pair(-1, 0),  // Up
        Pair(-1, -1), // Diagonal-left-up
        Pair(-1, 1)   // Diagonal-right-up
    )

    val rows = grid.size
    val cols = grid[0].length
    var count = 0

    for (r in 0 until rows) {
        for (c in 0 until cols) {
            for ((dx, dy) in directions) {
                if (checkWord(grid, r, c, dx, dy, targetWord, rows, cols)) {
                    count++
                }
            }
        }
    }

    return count
}

// Count all X-MAS patterns
fun countAllXMASPatterns(grid: List<String>): Int {
    val rows = grid.size
    val cols = grid[0].length
    var count = 0

    for (r in 1 until rows - 1) {
        for (c in 1 until cols - 1) {
            val center = grid[r][c]
            val topLeft = grid[r - 1][c - 1]
            val topRight = grid[r - 1][c + 1]
            val bottomLeft = grid[r + 1][c - 1]
            val bottomRight = grid[r + 1][c + 1]

            if (center == 'A') {
                // Pattern 1: M.S
                if (topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S') count++
                // Pattern 2: S.M
                if (topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M') count++
                // Pattern 3: M.M
                if (topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S') count++
                // Pattern 4: S.S
                if (topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M') count++
            }
        }
    }

    return count
}

// Main function
fun main() {
    val filePath = "input.txt"
    val grid = readGrid(filePath)

    if (grid.isEmpty()) {
        println("Error: Grid is empty or invalid.")
        return
    }

    val xmasCount = countXMAS(grid)
    println("Count of XMAS: $xmasCount")

    val xmasPatterns = countAllXMASPatterns(grid)
    println("Total X-MAS patterns: $xmasPatterns")
}
