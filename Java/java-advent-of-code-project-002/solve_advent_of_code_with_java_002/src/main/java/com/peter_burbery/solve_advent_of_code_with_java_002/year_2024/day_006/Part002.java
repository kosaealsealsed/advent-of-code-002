package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_006;

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Part002 {

    /**
     * Parses the grid from the given file and returns it as a list of character arrays.
     * Each line in the file represents a row in the grid.
     */
    public static List<char[]> parseGrid(String filePath) throws IOException {
        List<char[]> grid = new ArrayList<>();
        List<String> lines = Files.readAllLines(Paths.get(filePath));
        for (String line : lines) {
            grid.add(line.toCharArray());
        }
        return grid;
    }

    /**
     * Finds the guard's starting position and initial direction.
     * Returns a pair of (row, column) and direction as an integer.
     * Directions:
     * 0: Up (^)
     * 1: Right (>)
     * 2: Down (v)
     * 3: Left (<)
     */
    public static Guard findGuard(List<char[]> grid) {
        Map<Character, Integer> directionMap = Map.of(
                '^', 0,
                '>', 1,
                'v', 2,
                '<', 3
        );

        for (int r = 0; r < grid.size(); r++) {
            for (int c = 0; c < grid.get(r).length; c++) {
                char cell = grid.get(r)[c];
                if (directionMap.containsKey(cell)) {
                    return new Guard(new int[]{r, c}, directionMap.get(cell));
                }
            }
        }

        throw new IllegalArgumentException("Guard not found in the grid.");
    }

    /**
     * Returns a list of all possible positions where an obstruction can be placed.
     * Excludes the guard's starting position and already obstructed cells.
     */
    public static List<int[]> getPossibleObstructions(List<char[]> grid, int[] guardPos) {
        List<int[]> possible = new ArrayList<>();
        for (int r = 0; r < grid.size(); r++) {
            for (int c = 0; c < grid.get(r).length; c++) {
                if ((r != guardPos[0] || c != guardPos[1]) && grid.get(r)[c] == '.') {
                    possible.add(new int[]{r, c});
                }
            }
        }
        return possible;
    }

    /**
     * Simulates the guard's movement on the grid.
     * Returns true if a loop is detected, false if the guard exits the grid.
     */
    public static boolean simulateMovement(List<char[]> grid, int[] startPos, int startDir) {
        Map<Integer, int[]> directionOffsets = Map.of(
                0, new int[]{-1, 0}, // Up
                1, new int[]{0, 1},  // Right
                2, new int[]{1, 0},  // Down
                3, new int[]{0, -1} // Left
        );

        Set<String> visitedStates = new HashSet<>();
        int r = startPos[0];
        int c = startPos[1];
        int direction = startDir;

        while (true) {
            String state = r + "," + c + "," + direction;
            if (visitedStates.contains(state)) {
                return true; // Loop detected
            }
            visitedStates.add(state);

            int[] offset = directionOffsets.get(direction);
            int newR = r + offset[0];
            int newC = c + offset[1];

            // Check boundaries
            if (newR < 0 || newR >= grid.size() || newC < 0 || newC >= grid.get(0).length) {
                return false; // Guard exits the grid
            }

            if (grid.get(newR)[newC] == '#') {
                // Turn right if obstacle ahead
                direction = (direction + 1) % 4;
            } else {
                // Move forward
                r = newR;
                c = newC;
            }
        }
    }

    /**
     * Counts the number of positions where placing a single obstruction
     * causes the guard to loop indefinitely. Measures the execution time.
     */
    public static void countObstructionPositions(String filePath) throws IOException {
        long totalStartTime = System.nanoTime();

        // Parse the grid
        List<char[]> grid = parseGrid(filePath);

        // Find the guard's starting position and direction
        Guard guard = findGuard(grid);
        int[] guardPos = guard.position;
        int guardDir = guard.direction;

        // Find all possible obstruction positions
        long obstructionStartTime = System.nanoTime();
        List<int[]> possibleObstructions = getPossibleObstructions(grid, guardPos);
        long obstructionEndTime = System.nanoTime();

        double obstructionTime = (obstructionEndTime - obstructionStartTime) / 1e9;
        System.out.printf("time, denominator\n%.9f %d\n", obstructionTime, possibleObstructions.size());

        System.out.println("batch, batch time, cumulative time");

        // Batch processing
        int loopCount = 0;
        int total = possibleObstructions.size();
        int batchSize = 1000;
        long batchStartTime = System.nanoTime();
        double cumulativeTime = obstructionTime;

        for (int idx = 0; idx < total; idx++) {
            int[] obstruction = possibleObstructions.get(idx);
            int r = obstruction[0];
            int c = obstruction[1];
            grid.get(r)[c] = '#'; // Place obstruction

            if (simulateMovement(grid, guardPos, guardDir)) {
                loopCount++;
            }

            grid.get(r)[c] = '.'; // Remove obstruction

            // Print batch results
            if ((idx + 1) % batchSize == 0 || idx + 1 == total) {
                long batchEndTime = System.nanoTime();
                double batchTime = (batchEndTime - batchStartTime) / 1e9;
                cumulativeTime += batchTime;
                System.out.printf("%d %.9f %.9f\n", idx + 1, batchTime, cumulativeTime);
                batchStartTime = System.nanoTime();
            }
        }

        long totalEndTime = System.nanoTime();
        double totalTime = (totalEndTime - totalStartTime) / 1e9;

        System.out.printf("answer, answer time\n%d %.9f\n", loopCount, totalTime);
    }

    public static void main(String[] args) {
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt";

        try {
            countObstructionPositions(filePath);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    private static class Guard {
        int[] position;
        int direction;

        Guard(int[] position, int direction) {
            this.position = position;
            this.direction = direction;
        }
    }
}
