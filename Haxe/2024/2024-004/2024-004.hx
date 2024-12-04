import sys.io.File;
import StringTools;

class WordSearch {
    static public function main() {
        // File path and target word
        var filePath = "/uploads/input.txt";
        var targetWord = "XMAS";

        // Read the grid from the file
        var grid = File.getContent(filePath).split("\n").map(function(line) return StringTools.trim(line));
        var rows = grid.length;
        var cols = grid[0].length;

        // Directions for moving in the grid
        var directions = [
            {dx: 0, dy: 1},   // Right
            {dx: 1, dy: 0},   // Down
            {dx: 1, dy: 1},   // Diagonal-right-down
            {dx: 1, dy: -1},  // Diagonal-left-down
            {dx: 0, dy: -1},  // Left
            {dx: -1, dy: 0},  // Up
            {dx: -1, dy: -1}, // Diagonal-left-up
            {dx: -1, dy: 1}   // Diagonal-right-up
        ];

        // Function to check if a word exists in a given direction
        function checkWord(x:Int, y:Int, dx:Int, dy:Int):Bool {
            for (i in 0...targetWord.length) {
                var nx = x + i * dx;
                var ny = y + i * dy;
                if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx].charAt(ny) != targetWord.charAt(i)) {
                    return false;
                }
            }
            return true;
        }

        // Count all occurrences of the target word
        function countOccurrences():Int {
            var count = 0;
            for (r in 0...rows) {
                for (c in 0...cols) {
                    for (dir in directions) {
                        if (checkWord(r, c, dir.dx, dir.dy)) {
                            count++;
                        }
                    }
                }
            }
            return count;
        }

        // Count all XMAS patterns
        function countAllXmasPatterns():Int {
            var count = 0;
            for (r in 1...rows - 1) {
                for (c in 1...cols - 1) {
                    var center = grid[r].charAt(c);
                    var topLeft = grid[r - 1].charAt(c - 1);
                    var topRight = grid[r - 1].charAt(c + 1);
                    var bottomLeft = grid[r + 1].charAt(c - 1);
                    var bottomRight = grid[r + 1].charAt(c + 1);

                    // Check all valid XMAS configurations
                    if (center == 'A') {
                        if (topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S') {
                            count++;
                        } else if (topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M') {
                            count++;
                        } else if (topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S') {
                            count++;
                        } else if (topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M') {
                            count++;
                        }
                    }
                }
            }
            return count;
        }

        // Execute the counts
        var totalOccurrences = countOccurrences();
        trace("count is " + totalOccurrences);

        var totalXmasPatterns = countAllXmasPatterns();
        trace("total xmas patterns is " + totalXmasPatterns);
    }
}
