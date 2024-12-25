#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Function to check if the word exists in a given direction
bool checkWord(const char **grid, int x, int y, int dx, int dy, const char *word, int rows, int cols) {
    int len = strlen(word);
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
int countXMAS(const char **grid, int rows, int cols) {
    const char *targetWord = "XMAS";
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
int countAllXMASPatterns(const char **grid, int rows, int cols) {
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
    const char *filePath = "/uploads/input.txt";

    // Open the file
    FILE *file = fopen(filePath, "r");
    if (!file) {
        fprintf(stderr, "Error: Unable to open file.\n");
        return 1;
    }

    // Read the grid from the file
    char **grid = NULL;
    int rows = 0, cols = 0;
    char line[256];

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = '\0'; // Remove newline character
        if (cols == 0) {
            cols = strlen(line); // Determine the number of columns
        }

        // Allocate memory for the row
        grid = realloc(grid, sizeof(char *) * (rows + 1));
        grid[rows] = malloc((cols + 1) * sizeof(char));
        strcpy(grid[rows], line);
        rows++;
    }
    fclose(file);

    // Count occurrences of "XMAS"
    int xmasCount = countXMAS((const char **)grid, rows, cols);
    printf("Count of XMAS: %d\n", xmasCount);

    // Count all X-MAS patterns
    int xmasPatterns = countAllXMASPatterns((const char **)grid, rows, cols);
    printf("Total X-MAS patterns: %d\n", xmasPatterns);

    // Free memory
    for (int i = 0; i < rows; ++i) {
        free(grid[i]);
    }
    free(grid);

    return 0;
}
