#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

// Function to check if the word exists in a given direction
bool checkWord(const vector<string>& grid, int x, int y, int dx, int dy, const string& word, int rows, int cols) {
    for (size_t i = 0; i < word.size(); ++i) {
        int nx = x + i * dx;
        int ny = y + i * dy;
        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != word[i]) {
            return false;
        }
    }
    return true;
}

// Count occurrences of the word "XMAS" in all directions
int countXMAS(const vector<string>& grid, int rows, int cols) {
    string targetWord = "XMAS";
    vector<pair<int, int>> directions = {
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
            for (const auto& [dx, dy] : directions) {
                if (checkWord(grid, r, c, dx, dy, targetWord, rows, cols)) {
                    count++;
                }
            }
        }
    }
    return count;
}

// Count all X-MAS patterns
int countAllXMASPatterns(const vector<string>& grid, int rows, int cols) {
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
    string filePath = "input.txt";

    // Read the grid from the file
    ifstream file(filePath);
    if (!file.is_open()) {
        cerr << "Error: Unable to open file." << endl;
        return 1;
    }

    vector<string> grid;
    string line;
    while (getline(file, line)) {
        grid.push_back(line);
    }
    file.close();

    int rows = grid.size();
    int cols = grid[0].size();

    // Count occurrences of "XMAS"
    int xmasCount = countXMAS(grid, rows, cols);
    cout << "Count of XMAS: " << xmasCount << endl;

    // Count all X-MAS patterns
    int xmasPatterns = countAllXMASPatterns(grid, rows, cols);
    cout << "Total X-MAS patterns: " << xmasPatterns << endl;

    return 0;
}
