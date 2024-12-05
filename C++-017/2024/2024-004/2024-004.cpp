#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <stdexcept>

// Function to check if the word exists in a given direction
bool checkWord(const std::vector<std::string> &grid, int x, int y, int dx, int dy, const std::string &word, int rows, int cols) {
    int len = word.size();
    for (int i = 0; i < len; ++i) {
        int nx = x + i * dx;
        int ny = y + i * dy;
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != word[i]) {
            return false;
        }
    }
    return true;
}

// Count occurrences of the word "XMAS" in all directions
int countXMAS(const std::vector<std::string> &grid, int rows, int cols) {
    const std::string targetWord = "XMAS";
    int directions[8][2] = {
        {0, 1},   // Right
        {1, 0},   // Down
        {1, 1},   // Diagonal-right-down
        {1, -1},  // Diagonal-left-down
        {0, -1},  // Left
        {-1, 0},  // Up
        {-1, -1}, // Diagonal-left-up
        {-1, 1}   // Diagonal-right-up
    };

    int count = 0;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            for (int d = 0; d < 8; ++d) {
                int dx = directions[d][0];
                int dy = directions[d][1];
                if (checkWord(grid, r, c, dx, dy, targetWord, rows, cols)) {
                    count++;
                }
            }
        }
    }
    return count;
}

// Count all X-MAS patterns
int countAllXMASPatterns(const std::vector<std::string> &grid, int rows, int cols) {
    int count = 0;

    for (int r = 1; r < rows - 1; ++r) {
        for (int c = 1; c < cols - 1; ++c) {
            char center = grid[r][c];
            char topLeft = grid[r - 1][c - 1];
            char topRight = grid[r - 1][c + 1];
            char bottomLeft = grid[r + 1][c - 1];
            char bottomRight = grid[r + 1][c + 1];

            if (center == 'A') {
                // Pattern 1: M.S
                if (topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S') count++;
                // Pattern 2: S.M
                if (topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M') count++;
                // Pattern 3: M.M
                if (topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S') count++;
                // Pattern 4: S.S
                if (topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M') count++;
            }
        }
    }
    return count;
}

int main() {
    const std::string filePath = "/uploads/input.txt";

    // Open the file
    std::ifstream file(filePath);
    if (!file.is_open()) {
        std::cerr << "Error: Unable to open file." << std::endl;
        return 1;
    }

    // Read the grid from the file
    std::vector<std::string> grid;
    std::string line;

    while (std::getline(file, line)) {
        grid.push_back(line);
    }
    file.close();

    if (grid.empty()) {
        std::cerr << "Error: Grid is empty." << std::endl;
        return 1;
    }

    int rows = grid.size();
    int cols = grid[0].size();

    // Validate grid dimensions
    for (const auto &row : grid) {
        if (row.size() != cols) {
            throw std::runtime_error("Error: Grid rows have inconsistent lengths.");
        }
    }

    // Count occurrences of "XMAS"
    int xmasCount = countXMAS(grid, rows, cols);
    std::cout << "Count of XMAS: " << xmasCount << std::endl;

    // Count all X-MAS patterns
    int xmasPatterns = countAllXMASPatterns(grid, rows, cols);
    std::cout << "Total X-MAS patterns: " << xmasPatterns << std::endl;

    return 0;
}
