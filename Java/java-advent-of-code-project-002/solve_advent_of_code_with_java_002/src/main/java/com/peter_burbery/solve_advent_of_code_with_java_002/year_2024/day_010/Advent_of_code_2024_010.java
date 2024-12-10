package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_010;

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Advent_of_code_2024_010 {
    static final String INPUT_FILE = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-010\\input.txt";

    public static void main(String[] args) throws IOException {
        long startTimePart1 = System.nanoTime();
        List<List<Integer>> grid = readMap(INPUT_FILE);
        int totalScore = findTrailheadScores(grid);
        long endTimePart1 = System.nanoTime();
        System.out.printf("Part 1 Result: %d%n", totalScore);
        System.out.printf("Time taken for Part 1: %.9f s%n", (endTimePart1 - startTimePart1) / 1e9);

        long startTimePart2 = System.nanoTime();
        int totalRating = calculateTotalRating(grid);
        long endTimePart2 = System.nanoTime();
        System.out.printf("Part 2 Result: %d%n", totalRating);
        System.out.printf("Time taken for Part 2: %.9f s%n", (endTimePart2 - startTimePart2) / 1e9);
    }

    private static List<List<Integer>> readMap(String filename) throws IOException {
        List<List<Integer>> grid = new ArrayList<>();
        List<String> lines = Files.readAllLines(Paths.get(filename));
        for (String line : lines) {
            if (!line.trim().isEmpty()) {
                List<Integer> row = new ArrayList<>();
                for (char ch : line.trim().toCharArray()) {
                    row.add(Character.getNumericValue(ch));
                }
                grid.add(row);
            }
        }
        return grid;
    }

    private static List<int[]> neighbors(int r, int c, int rows, int cols) {
        List<int[]> result = new ArrayList<>();
        int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
        for (int[] dir : directions) {
            int nr = r + dir[0];
            int nc = c + dir[1];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                result.add(new int[]{nr, nc});
            }
        }
        return result;
    }

    private static int findTrailheadScores(List<List<Integer>> grid) {
        int rows = grid.size();
        int cols = rows > 0 ? grid.get(0).size() : 0;

        List<int[]> trailheads = new ArrayList<>();
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid.get(r).get(c) == 0) {
                    trailheads.add(new int[]{r, c});
                }
            }
        }

        int totalScore = 0;

        for (int[] start : trailheads) {
            int startR = start[0];
            int startC = start[1];

            Set<String> visited = new HashSet<>();
            Queue<int[]> queue = new LinkedList<>();
            queue.add(new int[]{startR, startC});
            visited.add(startR + "," + startC);

            Set<String> reachableNines = new HashSet<>();

            while (!queue.isEmpty()) {
                int[] current = queue.poll();
                int r = current[0];
                int c = current[1];
                int currentHeight = grid.get(r).get(c);

                if (currentHeight == 9) {
                    reachableNines.add(r + "," + c);
                } else {
                    int nextHeight = currentHeight + 1;
                    for (int[] neighbor : neighbors(r, c, rows, cols)) {
                        int nr = neighbor[0];
                        int nc = neighbor[1];
                        if (!visited.contains(nr + "," + nc) && grid.get(nr).get(nc) == nextHeight) {
                            visited.add(nr + "," + nc);
                            queue.add(new int[]{nr, nc});
                        }
                    }
                }
            }

            totalScore += reachableNines.size();
        }

        return totalScore;
    }

    private static int calculateTotalRating(List<List<Integer>> grid) {
        int rows = grid.size();
        int cols = rows > 0 ? grid.get(0).size() : 0;

        List<int[]> trailheads = new ArrayList<>();
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid.get(r).get(c) == 0) {
                    trailheads.add(new int[]{r, c});
                }
            }
        }

        int[][] dp = new int[rows][cols];
        for (int[] row : dp) {
            Arrays.fill(row, -1);
        }

        int totalRating = 0;
        for (int[] trailhead : trailheads) {
            totalRating += countPaths(trailhead[0], trailhead[1], grid, dp, rows, cols);
        }

        return totalRating;
    }

    private static int countPaths(int r, int c, List<List<Integer>> grid, int[][] dp, int rows, int cols) {
        if (dp[r][c] != -1) {
            return dp[r][c];
        }

        int currentHeight = grid.get(r).get(c);

        if (currentHeight == 9) {
            dp[r][c] = 1;
            return 1;
        }

        int totalPaths = 0;
        int nextHeight = currentHeight + 1;
        for (int[] neighbor : neighbors(r, c, rows, cols)) {
            int nr = neighbor[0];
            int nc = neighbor[1];
            if (grid.get(nr).get(nc) == nextHeight) {
                totalPaths += countPaths(nr, nc, grid, dp, rows, cols);
            }
        }

        dp[r][c] = totalPaths;
        return totalPaths;
    }
}

