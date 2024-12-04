package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_004;

import java.io.*;
import java.util.*;

public class XMASSolver {

    public static void main(String[] args) throws IOException {
        // Read the file and store the grid
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt";
        List<String> gridList = new ArrayList<>();
        
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                gridList.add(line);
            }
        }
        
        String[] grid = gridList.toArray(new String[0]);
        int rows = grid.length;
        int cols = grid[0].length();

        // Count occurrences of "XMAS"
        int xmasCount = countXMAS(grid, rows, cols);
        System.out.println("Count of XMAS: " + xmasCount);

        // Count all X-MAS patterns
        int xmasPatterns = countAllXMASPatterns(grid, rows, cols);
        System.out.println("Total X-MAS patterns: " + xmasPatterns);
    }

    // Count occurrences of "XMAS" in all directions
    public static int countXMAS(String[] grid, int rows, int cols) {
        String targetWord = "XMAS";
        int count = 0;
        int[][] directions = {
            {0, 1},  // Right
            {1, 0},  // Down
            {1, 1},  // Diagonal-right-down
            {1, -1}, // Diagonal-left-down
            {0, -1}, // Left
            {-1, 0}, // Up
            {-1, -1},// Diagonal-left-up
            {-1, 1}  // Diagonal-right-up
        };

        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                for (int[] dir : directions) {
                    if (checkWord(grid, rows, cols, r, c, dir[0], dir[1], targetWord)) {
                        count++;
                    }
                }
            }
        }
        return count;
    }

    // Check if a word exists in a given direction
    public static boolean checkWord(String[] grid, int rows, int cols, int x, int y, int dx, int dy, String targetWord) {
        for (int i = 0; i < targetWord.length(); i++) {
            int nx = x + i * dx;
            int ny = y + i * dy;
            if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx].charAt(ny) != targetWord.charAt(i)) {
                return false;
            }
        }
        return true;
    }

    // Count all X-MAS patterns
    public static int countAllXMASPatterns(String[] grid, int rows, int cols) {
        int count = 0;
        for (int r = 1; r < rows - 1; r++) {
            for (int c = 1; c < cols - 1; c++) {
                char center = grid[r].charAt(c);
                char topLeft = grid[r - 1].charAt(c - 1);
                char topRight = grid[r - 1].charAt(c + 1);
                char bottomLeft = grid[r + 1].charAt(c - 1);
                char bottomRight = grid[r + 1].charAt(c + 1);

                if (center == 'A') {
                    // Pattern 1: M.S
                    if (topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S') {
                        count++;
                    }
                    // Pattern 2: S.M
                    else if (topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M') {
                        count++;
                    }
                    // Pattern 3: M.M
                    else if (topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S') {
                        count++;
                    }
                    // Pattern 4: S.S
                    else if (topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M') {
                        count++;
                    }
                }
            }
        }
        return count;
    }
}

