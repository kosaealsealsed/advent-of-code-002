package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_006;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class GuardPatrolLoopDetector {

    // Direction mappings
    private static final Map<Character, Integer> DIRECTION_MAP = Map.of(
            '^', 0,
            '>', 1,
            'v', 2,
            '<', 3
    );

    private static final int[][] DIRECTION_OFFSETS = {
            {-1, 0}, // Up
            {0, 1},  // Right
            {1, 0},  // Down
            {0, -1}  // Left
    };

    public static void main(String[] args) {
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt"; // Specify the path to your input file here
        try {
            // Part 1: Count distinct positions visited without obstructions
            int distinctPositions = countDistinctPositionsVisited(filePath);
            System.out.println("Number of distinct positions visited: " + distinctPositions);

            // Part 2: Detect loops with obstructions and measure execution times
            countObstructionPositions(filePath);
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    /**
     * Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
     *
     * @param filePath Path to the input file.
     * @return Number of distinct positions visited.
     * @throws IOException If the file cannot be read.
     */
    private static int countDistinctPositionsVisited(String filePath) throws IOException {
        // Parse the grid
        char[][] grid = parseGrid(filePath);

        // Find the guard's starting position and direction
        Pair<Position, Integer> guardInfo = findGuard(grid);
        Position guardPos = guardInfo.first;
        int guardDir = guardInfo.second;

        // Initialize visited positions set
        Set<Position> visitedPositions = new HashSet<>();
        visitedPositions.add(guardPos);

        // Simulate the guard's movement
        while (true) {
            int dr = DIRECTION_OFFSETS[guardDir][0];
            int dc = DIRECTION_OFFSETS[guardDir][1];
            int newR = guardPos.row + dr;
            int newC = guardPos.col + dc;

            // Check boundaries
            if (newR < 0 || newR >= grid.length || newC < 0 || newC >= grid[0].length) {
                break; // Guard exits the mapped area
            }

            if (grid[newR][newC] == '#') {
                // Turn right if obstacle ahead
                guardDir = (guardDir + 1) % 4;
            } else {
                // Move forward
                guardPos = new Position(newR, newC);
                visitedPositions.add(guardPos);
            }
        }

        // Number of distinct positions visited
        return visitedPositions.size();
    }

    /**
     * Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
     * Also measures and prints execution times.
     *
     * @param filePath Path to the input file.
     * @throws IOException If the file cannot be read.
     */
    private static void countObstructionPositions(String filePath) throws IOException {
        // Start total timing
        long totalStartTime = System.nanoTime();

        // Parse the grid
        char[][] grid = parseGrid(filePath);

        // Find the guard's starting position and direction
        Pair<Position, Integer> guardInfo = findGuard(grid);
        Position guardPos = guardInfo.first;
        int guardDir = guardInfo.second;

        // Time to find obstruction positions
        long obstructionStartTime = System.nanoTime();
        List<Position> possibleObstructions = getPossibleObstructions(grid, guardPos);
        long obstructionEndTime = System.nanoTime();
        double obstructionTime = (obstructionEndTime - obstructionStartTime) / 1_000_000_000.0;

        // Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
        System.out.println("time, denominator");
        System.out.printf("%.9f %d%n", obstructionTime, possibleObstructions.size());

        // Print header for batches
        System.out.println("batch, batch time, cumulative time");

        // Initialize loop counter
        int loopCount = 0;
        int total = possibleObstructions.size();

        // Initialize timing for batches
        final int batchSize = 1000;
        long batchStartTime = System.nanoTime();
        double cumulativeTime = obstructionTime; // cumulative_time includes obstruction_time

        for (int idx = 0; idx < possibleObstructions.size(); idx++) {
            Position obstruction = possibleObstructions.get(idx);
            grid[obstruction.row][obstruction.col] = '#'; // Place obstruction

            if (simulateMovement(grid, guardPos, guardDir)) {
                loopCount++; // Found a position that causes a loop
            }

            grid[obstruction.row][obstruction.col] = '.'; // Remove obstruction

            // Check if batch size is reached or it's the last position
            if ((idx + 1) % batchSize == 0 || (idx + 1) == total) {
                long batchEndTime = System.nanoTime();
                double batchTime = (batchEndTime - batchStartTime) / 1_000_000_000.0;
                cumulativeTime += batchTime;
                System.out.printf("%d %.9f %.9f%n", idx + 1, batchTime, cumulativeTime);
                batchStartTime = System.nanoTime(); // Reset batch start time
            }
        }

        // End total timing
        long totalEndTime = System.nanoTime();
        double totalTime = (totalEndTime - totalStartTime) / 1_000_000_000.0; // Total time from start to end

        // Print final answer header and line: [answer] [answer_time]
        System.out.println("answer, answer time");
        System.out.printf("%d %.9f%n", loopCount, totalTime);
    }

    /**
     * Parses the grid from the given file.
     *
     * @param filePath Path to the input file.
     * @return 2D character array representing the grid.
     * @throws IOException If the file cannot be read.
     */
    private static char[][] parseGrid(String filePath) throws IOException {
        List<char[]> gridList = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                gridList.add(line.trim().toCharArray());
            }
        }
        if (gridList.isEmpty()) {
            throw new IllegalArgumentException("The grid is empty.");
        }
        // Ensure all rows have the same length
        int cols = gridList.get(0).length;
        for (char[] row : gridList) {
            if (row.length != cols) {
                throw new IllegalArgumentException("Inconsistent row lengths in the grid.");
            }
        }
        return gridList.toArray(new char[0][]);
    }

    /**
     * Finds the guard's starting position and direction.
     *
     * @param grid 2D character array representing the grid.
     * @return A Pair containing the starting Position and direction.
     */
    private static Pair<Position, Integer> findGuard(char[][] grid) {
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[0].length; c++) {
                char cell = grid[r][c];
                if (DIRECTION_MAP.containsKey(cell)) {
                    Position guardPos = new Position(r, c);
                    int guardDir = DIRECTION_MAP.get(cell);
                    grid[r][c] = '.'; // Clear the starting position
                    return new Pair<>(guardPos, guardDir);
                }
            }
        }
        throw new IllegalArgumentException("Guard not found in the grid.");
    }

    /**
     * Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
     *
     * @param grid     2D character array representing the grid.
     * @param guardPos The starting position of the guard.
     * @return List of possible obstruction Positions.
     */
    private static List<Position> getPossibleObstructions(char[][] grid, Position guardPos) {
        List<Position> possible = new ArrayList<>();
        for (int r = 0; r < grid.length; r++) {
            for (int c = 0; c < grid[0].length; c++) {
                if ((r != guardPos.row || c != guardPos.col) && grid[r][c] == '.') {
                    possible.add(new Position(r, c));
                }
            }
        }
        return possible;
    }

    /**
     * Simulates the guard's movement on the grid.
     *
     * @param grid     2D character array representing the grid.
     * @param startPos Starting position of the guard.
     * @param startDir Starting direction of the guard.
     * @return True if a loop is detected, False if the guard exits the grid.
     */
    private static boolean simulateMovement(char[][] grid, Position startPos, int startDir) {
        Set<State> visitedStates = new HashSet<>();
        int r = startPos.row;
        int c = startPos.col;
        int direction = startDir;

        while (true) {
            State currentState = new State(r, c, direction);
            if (visitedStates.contains(currentState)) {
                return true; // Loop detected
            }
            visitedStates.add(currentState);

            int dr = DIRECTION_OFFSETS[direction][0];
            int dc = DIRECTION_OFFSETS[direction][1];
            int newR = r + dr;
            int newC = c + dc;

            // Check boundaries
            if (newR < 0 || newR >= grid.length || newC < 0 || newC >= grid[0].length) {
                return false; // Guard exits the grid
            }

            if (grid[newR][newC] == '#') {
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
     * Helper class to represent a position in the grid.
     */
    private static class Position {
        int row;
        int col;

        Position(int r, int c) {
            this.row = r;
            this.col = c;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (!(obj instanceof Position)) return false;
            Position other = (Position) obj;
            return this.row == other.row && this.col == other.col;
        }

        @Override
        public int hashCode() {
            return Objects.hash(row, col);
        }
    }

    /**
     * Helper class to represent a state (position and direction) of the guard.
     */
    private static class State {
        int row;
        int col;
        int direction;

        State(int r, int c, int dir) {
            this.row = r;
            this.col = c;
            this.direction = dir;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) return true;
            if (!(obj instanceof State)) return false;
            State other = (State) obj;
            return this.row == other.row && this.col == other.col && this.direction == other.direction;
        }

        @Override
        public int hashCode() {
            return Objects.hash(row, col, direction);
        }
    }

    /**
     * Simple Pair class to hold two related objects.
     *
     * @param <F> Type of the first element.
     * @param <S> Type of the second element.
     */
    private static class Pair<F, S> {
        F first;
        S second;

        Pair(F f, S s) {
            this.first = f;
            this.second = s;
        }
    }
}
