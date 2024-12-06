package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_006;

import java.io.*;
import java.util.*;

public class Part001 {

    public static void main(String[] args) throws IOException {
        // Start timing
        long startTime = System.nanoTime();

        // Load the input data from the file
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt";
        List<List<Character>> maze = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                List<Character> row = new ArrayList<>();
                for (char ch : line.toCharArray()) {
                    row.add(ch);
                }
                maze.add(row);
            }
        }

        // Constants for directions and turning
        Map<Character, int[]> DIRECTIONS = Map.of(
                '^', new int[]{-1, 0},
                '>', new int[]{0, 1},
                'v', new int[]{1, 0},
                '<', new int[]{0, -1}
        );

        Map<Character, Character> TURN_RIGHT = Map.of(
                '^', '>',
                '>', 'v',
                'v', '<',
                '<', '^'
        );

        // Find the initial position and direction of the guard
        int rows = maze.size();
        int cols = maze.get(0).size();
        int[] guardPos = null;
        char guardDir = ' ';
        
        outerLoop:
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                char cell = maze.get(r).get(c);
                if (DIRECTIONS.containsKey(cell)) {
                    guardPos = new int[]{r, c};
                    guardDir = cell;
                    maze.get(r).set(c, '.'); // Clear the starting position
                    break outerLoop;
                }
            }
        }

        if (guardPos == null) {
            System.out.println("No guard found in the maze.");
            return;
        }

        // Track the positions visited
        Set<String> visitedPositions = new HashSet<>();
        visitedPositions.add(Arrays.toString(guardPos));

        // Simulate the guard's movement
        while (true) {
            int nextR = guardPos[0] + DIRECTIONS.get(guardDir)[0];
            int nextC = guardPos[1] + DIRECTIONS.get(guardDir)[1];

            // Guard leaves the mapped area
            if (nextR < 0 || nextR >= rows || nextC < 0 || nextC >= cols) {
                break;
            }

            if (maze.get(nextR).get(nextC) == '#') { // Obstacle ahead, turn right
                guardDir = TURN_RIGHT.get(guardDir);
            } else { // Move forward
                guardPos[0] = nextR;
                guardPos[1] = nextC;
                visitedPositions.add(Arrays.toString(guardPos));
            }
        }

        // End timing
        long endTime = System.nanoTime();

        // Calculate distinct positions visited
        int distinctPositions = visitedPositions.size();

        // Calculate elapsed time in seconds with precision
        double elapsedTime = (endTime - startTime) / 1_000_000_000.0;

        // Format the elapsed time with three spaces as separators
        String formattedTime = String.format("%,.9f", elapsedTime).replace(",", " ");

        // Print the header row
        System.out.println("answer, time");

        // Print the result in the desired format
        System.out.println(distinctPositions + ", " + formattedTime);

    }
}
