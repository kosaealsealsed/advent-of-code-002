using System;
using System.Collections.Generic;
using System.IO;

class XMASSolver
{
    static void Main(string[] args)
    {
        string filePath = "input.txt";

        // Read the grid from the file
        var grid = File.ReadAllLines(filePath);
        int rows = grid.Length;
        int cols = grid[0].Length;

        // Count occurrences of "XMAS"
        int xmasCount = CountXMAS(grid, rows, cols);
        Console.WriteLine($"Count of XMAS: {xmasCount}");

        // Count all X-MAS patterns
        int xmasPatterns = CountAllXMASPatterns(grid, rows, cols);
        Console.WriteLine($"Total X-MAS patterns: {xmasPatterns}");
    }

    // Count occurrences of "XMAS" in all directions
    static int CountXMAS(string[] grid, int rows, int cols)
    {
        string targetWord = "XMAS";
        var directions = new (int dx, int dy)[]
        {
            (0, 1),   // Right
            (1, 0),   // Down
            (1, 1),   // Diagonal-right-down
            (1, -1),  // Diagonal-left-down
            (0, -1),  // Left
            (-1, 0),  // Up
            (-1, -1), // Diagonal-left-up
            (-1, 1)   // Diagonal-right-up
        };

        int count = 0;

        for (int r = 0; r < rows; r++)
        {
            for (int c = 0; c < cols; c++)
            {
                foreach (var direction in directions)
                {
                    int dx = direction.dx;
                    int dy = direction.dy;
                    if (CheckWord(grid, r, c, dx, dy, targetWord, rows, cols))
                    {
                        count++;
                    }
                }
            }
        }

        return count;
    }

    // Check if a word exists in the grid in the given direction
    static bool CheckWord(string[] grid, int x, int y, int dx, int dy, string word, int rows, int cols)
    {
        for (int i = 0; i < word.Length; i++)
        {
            int nx = x + i * dx;
            int ny = y + i * dy;

            if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != word[i])
            {
                return false;
            }
        }
        return true;
    }

    // Count all X-MAS patterns
    static int CountAllXMASPatterns(string[] grid, int rows, int cols)
    {
        int count = 0;

        for (int r = 1; r < rows - 1; r++)
        {
            for (int c = 1; c < cols - 1; c++)
            {
                char center = grid[r][c];
                char topLeft = grid[r - 1][c - 1];
                char topRight = grid[r - 1][c + 1];
                char bottomLeft = grid[r + 1][c - 1];
                char bottomRight = grid[r + 1][c + 1];

                if (center == 'A')
                {
                    // Pattern 1: M.S
                    if (topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S')
                        count++;
                    // Pattern 2: S.M
                    else if (topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M')
                        count++;
                    // Pattern 3: M.M
                    else if (topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S')
                        count++;
                    // Pattern 4: S.S
                    else if (topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M')
                        count++;
                }
            }
        }

        return count;
    }
}
