import 'dart:io';

void main() {
  // File path and target word
  const filePath = '/uploads/input.txt';
  const targetWord = 'XMAS';

  // Read the grid from the file
  final grid = File(filePath).readAsLinesSync();

  // Grid dimensions
  final rows = grid.length;
  final cols = grid[0].length;

  // Directions for moving in the grid
  final directions = [
    [0, 1],   // Right
    [1, 0],   // Down
    [1, 1],   // Diagonal-right-down
    [1, -1],  // Diagonal-left-down
    [0, -1],  // Left
    [-1, 0],  // Up
    [-1, -1], // Diagonal-left-up
    [-1, 1]   // Diagonal-right-up
  ];

  // Function to check if a word exists in a given direction
  bool checkWord(int x, int y, int dx, int dy) {
    for (int i = 0; i < targetWord.length; i++) {
      final nx = x + i * dx;
      final ny = y + i * dy;
      if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != targetWord[i]) {
        return false;
      }
    }
    return true;
  }

  // Count all occurrences of the target word
  int countOccurrences() {
    int count = 0;
    for (int r = 0; r < rows; r++) {
      for (int c = 0; c < cols; c++) {
        for (var dir in directions) {
          if (checkWord(r, c, dir[0], dir[1])) {
            count++;
          }
        }
      }
    }
    return count;
  }

  // Function to count all XMAS patterns
  int countAllXmasPatterns() {
    int count = 0;
    for (int r = 1; r < rows - 1; r++) {
      for (int c = 1; c < cols - 1; c++) {
        final center = grid[r][c];
        final topLeft = grid[r - 1][c - 1];
        final topRight = grid[r - 1][c + 1];
        final bottomLeft = grid[r + 1][c - 1];
        final bottomRight = grid[r + 1][c + 1];

        // Check all valid X-MAS configurations
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

  final totalOccurrences = countOccurrences();
  print('count is $totalOccurrences.');

  final totalXmasPatterns = countAllXmasPatterns();
  print('total xmas patterns is $totalXmasPatterns.');
}
