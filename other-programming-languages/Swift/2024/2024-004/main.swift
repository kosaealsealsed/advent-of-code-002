import Foundation

// Function to read the file and return the grid as an array of strings
func readFile(_ filePath: String) -> [String] {
    do {
        let contents = try String(contentsOfFile: filePath, encoding: .utf8)
        return contents.split(separator: "\n").map { String($0) }
    } catch {
        print("Error reading file: \(error)")
        return []
    }
}

// Function to check if a word exists in a given direction
func checkWord(grid: [[Character]], word: String, x: Int, y: Int, dx: Int, dy: Int) -> Bool {
    let rows = grid.count
    let cols = grid[0].count
    for (i, char) in word.enumerated() {
        let nx = x + i * dx
        let ny = y + i * dy
        if nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != char {
            return false
        }
    }
    return true
}

// Count all occurrences of the target word
func countOccurrences(grid: [[Character]], word: String) -> Int {
    let directions = [
        (0, 1),   // Right
        (1, 0),   // Down
        (1, 1),   // Diagonal-right-down
        (1, -1),  // Diagonal-left-down
        (0, -1),  // Left
        (-1, 0),  // Up
        (-1, -1), // Diagonal-left-up
        (-1, 1)   // Diagonal-right-up
    ]
    let rows = grid.count
    let cols = grid[0].count
    var count = 0

    for r in 0..<rows {
        for c in 0..<cols {
            for (dx, dy) in directions {
                if checkWord(grid: grid, word: word, x: r, y: c, dx: dx, dy: dy) {
                    count += 1
                }
            }
        }
    }

    return count
}

// Count all X-MAS patterns
func countXmasPatterns(grid: [[Character]]) -> Int {
    let rows = grid.count
    let cols = grid[0].count
    var count = 0

    for r in 1..<rows - 1 {
        for c in 1..<cols - 1 {
            let center = grid[r][c]
            let topLeft = grid[r - 1][c - 1]
            let topRight = grid[r - 1][c + 1]
            let bottomLeft = grid[r + 1][c - 1]
            let bottomRight = grid[r + 1][c + 1]

            if center == "A" {
                if (topLeft == "M" && topRight == "S" && bottomLeft == "M" && bottomRight == "S") ||
                   (topLeft == "S" && topRight == "M" && bottomLeft == "S" && bottomRight == "M") ||
                   (topLeft == "M" && topRight == "M" && bottomLeft == "S" && bottomRight == "S") ||
                   (topLeft == "S" && topRight == "S" && bottomLeft == "M" && bottomRight == "M") {
                    count += 1
                }
            }
        }
    }

    return count
}

// Main logic
let filePath = "input.txt"
let gridStrings = readFile(filePath)
let grid = gridStrings.map { Array($0) }
let targetWord = "XMAS"

// Count occurrences of the word "XMAS"
let wordCount = countOccurrences(grid: grid, word: targetWord)
print("count is \(wordCount)")

// Count all X-MAS patterns
let xmasPatternCount = countXmasPatterns(grid: grid)
print("total xmas patterns is \(xmasPatternCount)")
