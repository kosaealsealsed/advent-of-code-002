package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_018.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class LikeARoguePart001 {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_018\\input.txt";

        try {
            String initialRow = readInput(filePath);

            // Timing start
            long startTime = System.nanoTime();

            // Calculate the number of safe tiles for 40 rows
            int rows = 40;
            int safeTileCount = countSafeTiles(initialRow, rows);

            // Timing end
            long endTime = System.nanoTime();
            double duration = (endTime - startTime) / 1_000_000_000.0;

            System.out.println("Number of safe tiles in 40 rows: " + safeTileCount);
            System.out.printf("Execution Time: %.9f s%n", duration);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static String readInput(String filePath) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            return reader.readLine().trim();
        }
    }

    private static int countSafeTiles(String initialRow, int totalRows) {
        int rowLength = initialRow.length();
        char[] currentRow = initialRow.toCharArray();
        char[] nextRow = new char[rowLength];
        int safeTileCount = 0;

        // Count safe tiles in the initial row
        for (char tile : currentRow) {
            if (tile == '.') {
                safeTileCount++;
            }
        }

        // Generate subsequent rows
        for (int row = 1; row < totalRows; row++) {
            for (int i = 0; i < rowLength; i++) {
                // Determine left, center, and right tiles
                char left = (i == 0) ? '.' : currentRow[i - 1];
                char center = currentRow[i];
                char right = (i == rowLength - 1) ? '.' : currentRow[i + 1];

                // Apply trap rules
                if ((left == '^' && center == '^' && right == '.') ||
                    (center == '^' && right == '^' && left == '.') ||
                    (left == '^' && center == '.' && right == '.') ||
                    (right == '^' && center == '.' && left == '.')) {
                    nextRow[i] = '^';
                } else {
                    nextRow[i] = '.';
                    safeTileCount++;
                }
            }

            // Move to the next row
            char[] temp = currentRow;
            currentRow = nextRow;
            nextRow = temp; // Reuse the array to save memory
        }

        return safeTileCount;
    }
}
