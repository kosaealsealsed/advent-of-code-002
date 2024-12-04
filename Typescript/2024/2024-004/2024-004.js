"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
// Function to read the grid from the input file
function readGrid(filePath) {
    try {
        var data = fs.readFileSync(filePath, "utf8");
        return data.split("\n").map(function (line) { return line.trim(); }).filter(function (line) { return line.length > 0; });
    }
    catch (err) {
        console.error("Error reading file:", err);
        return [];
    }
}
// Function to check if the word exists in a given direction
function checkWord(grid, x, y, dx, dy, word, rows, cols) {
    for (var i = 0; i < word.length; i++) {
        var nx = x + i * dx;
        var ny = y + i * dy;
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] !== word[i]) {
            return false;
        }
    }
    return true;
}
// Count occurrences of the word "XMAS" in all directions
function countXMAS(grid) {
    var targetWord = "XMAS";
    var directions = [
        [0, 1], // Right
        [1, 0], // Down
        [1, 1], // Diagonal-right-down
        [1, -1], // Diagonal-left-down
        [0, -1], // Left
        [-1, 0], // Up
        [-1, -1], // Diagonal-left-up
        [-1, 1] // Diagonal-right-up
    ];
    var rows = grid.length;
    var cols = grid[0].length;
    var count = 0;
    for (var r = 0; r < rows; r++) {
        for (var c = 0; c < cols; c++) {
            for (var _i = 0, directions_1 = directions; _i < directions_1.length; _i++) {
                var _a = directions_1[_i], dx = _a[0], dy = _a[1];
                if (checkWord(grid, r, c, dx, dy, targetWord, rows, cols)) {
                    count++;
                }
            }
        }
    }
    return count;
}
// Count all X-MAS patterns
function countAllXMASPatterns(grid) {
    var rows = grid.length;
    var cols = grid[0].length;
    var count = 0;
    for (var r = 1; r < rows - 1; r++) {
        for (var c = 1; c < cols - 1; c++) {
            var center = grid[r][c];
            var topLeft = grid[r - 1][c - 1];
            var topRight = grid[r - 1][c + 1];
            var bottomLeft = grid[r + 1][c - 1];
            var bottomRight = grid[r + 1][c + 1];
            if (center === 'A') {
                // Pattern 1: M.S
                if (topLeft === 'M' && topRight === 'S' && bottomLeft === 'M' && bottomRight === 'S')
                    count++;
                // Pattern 2: S.M
                if (topLeft === 'S' && topRight === 'M' && bottomLeft === 'S' && bottomRight === 'M')
                    count++;
                // Pattern 3: M.M
                if (topLeft === 'M' && topRight === 'M' && bottomLeft === 'S' && bottomRight === 'S')
                    count++;
                // Pattern 4: S.S
                if (topLeft === 'S' && topRight === 'S' && bottomLeft === 'M' && bottomRight === 'M')
                    count++;
            }
        }
    }
    return count;
}
// Main function
function main() {
    var filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt";
    var grid = readGrid(filePath);
    if (grid.length === 0) {
        console.error("Error: Grid is empty or invalid.");
        return;
    }
    var xmasCount = countXMAS(grid);
    console.log("Count of XMAS: ".concat(xmasCount));
    var xmasPatterns = countAllXMASPatterns(grid);
    console.log("Total X-MAS patterns: ".concat(xmasPatterns));
}
// Run the main function
main();
