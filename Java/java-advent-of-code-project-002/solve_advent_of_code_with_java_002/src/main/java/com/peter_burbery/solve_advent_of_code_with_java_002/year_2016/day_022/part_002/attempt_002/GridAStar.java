package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_022.part_002.attempt_002;

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class GridAStar {

    static class Node {
        int x, y, size, used, avail;

        Node(int x, int y, int size, int used, int avail) {
            this.x = x;
            this.y = y;
            this.size = size;
            this.used = used;
            this.avail = avail;
        }
    }

    static class State implements Comparable<State> {
        int steps;
        int heuristic;
        int[][] grid;
        int[] emptyPos;
        int[] goalPos;

        State(int steps, int heuristic, int[][] grid, int[] emptyPos, int[] goalPos) {
            this.steps = steps;
            this.heuristic = heuristic;
            this.grid = grid;
            this.emptyPos = emptyPos;
            this.goalPos = goalPos;
        }

        @Override
        public int compareTo(State other) {
            return Integer.compare(this.steps + this.heuristic, other.steps + other.heuristic);
        }
    }

    private static int heuristic(int[] goalPos) {
        return Math.abs(goalPos[0]) + Math.abs(goalPos[1]);
    }

    public static int findMinSteps(int[][] grid, int[] emptyNode, int[] goalData, int maxX, int maxY) {
        PriorityQueue<State> queue = new PriorityQueue<>();
        Set<String> visited = new HashSet<>();

        long startTime = System.nanoTime(); // Start timing

        queue.add(new State(0, heuristic(goalData), grid, emptyNode, goalData));

        while (!queue.isEmpty()) {
            State current = queue.poll();

            if (current.goalPos[0] == 0 && current.goalPos[1] == 0) {
                long endTime = System.nanoTime(); // End timing
                logExecutionTime(startTime, endTime);
                return current.steps;
            }

            String key = Arrays.deepToString(current.grid) + Arrays.toString(current.emptyPos) + Arrays.toString(current.goalPos);
            if (visited.contains(key)) {
                continue;
            }
            visited.add(key);

            int[] emptyPos = current.emptyPos;
            int xEmpty = emptyPos[0];
            int yEmpty = emptyPos[1];

            int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
            for (int[] dir : directions) {
                int nxEmpty = xEmpty + dir[0];
                int nyEmpty = yEmpty + dir[1];

                if (nxEmpty >= 0 && nxEmpty <= maxX && nyEmpty >= 0 && nyEmpty <= maxY && grid[nyEmpty][nxEmpty] != -1) {
                    int[][] newGrid = deepCopyGrid(current.grid);
                    newGrid[yEmpty][xEmpty] = newGrid[nyEmpty][nxEmpty];
                    newGrid[nyEmpty][nxEmpty] = 0;

                    int[] newGoalPos = current.goalPos.clone();
                    if (nxEmpty == newGoalPos[0] && nyEmpty == newGoalPos[1]) {
                        newGoalPos[0] = xEmpty;
                        newGoalPos[1] = yEmpty;
                    }

                    int newHeuristic = heuristic(newGoalPos);
                    queue.add(new State(current.steps + 1, newHeuristic, newGrid, new int[]{nxEmpty, nyEmpty}, newGoalPos));
                }
            }
        }

        long endTime = System.nanoTime(); // End timing if goal is unreachable
        logExecutionTime(startTime, endTime);

        return -1;
    }

    private static int[][] deepCopyGrid(int[][] grid) {
        int[][] copy = new int[grid.length][];
        for (int i = 0; i < grid.length; i++) {
            copy[i] = grid[i].clone();
        }
        return copy;
    }

    private static void logExecutionTime(long startTime, long endTime) {
        long duration = endTime - startTime;
        long seconds = duration / 1_000_000_000;
        long nanos = duration % 1_000_000_000;
        System.out.printf("Execution time: %d.%09d s%n", seconds, nanos);
    }

        public static void main(String[] args) throws IOException {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_022\\input.txt";

        List<String> lines;
        // Use Files.readAllLines to read the file
        lines = Files.readAllLines(Paths.get(filePath));

        Map<int[], Node> nodes = new HashMap<>();
        int maxX = 0, maxY = 0;

        // Parse input
        for (String line : lines) {
            if (line.startsWith("/dev/grid/")) {
                String[] parts = line.split("\\s+");
                String[] pos = parts[0].split("-");
                int x = Integer.parseInt(pos[1].substring(1));
                int y = Integer.parseInt(pos[2].substring(1));
                int size = Integer.parseInt(parts[1].replace("T", ""));
                int used = Integer.parseInt(parts[2].replace("T", ""));
                int avail = Integer.parseInt(parts[3].replace("T", ""));

                nodes.put(new int[]{x, y}, new Node(x, y, size, used, avail));
                maxX = Math.max(maxX, x);
                maxY = Math.max(maxY, y);
            }
        }

        // Build the grid
        int[][] grid = new int[maxY + 1][maxX + 1];
        int[] emptyNode = null;
        int[] goalData = null;

        for (Map.Entry<int[], Node> entry : nodes.entrySet()) {
            int[] pos = entry.getKey();
            Node node = entry.getValue();

            if (node.used == 0) {
                grid[pos[1]][pos[0]] = 0;
                emptyNode = pos;
            } else if (node.used > 100) {
                grid[pos[1]][pos[0]] = -1;
            } else {
                grid[pos[1]][pos[0]] = 1;
            }

            if (pos[1] == 0 && pos[0] == maxX) {
                goalData = pos;
            }
        }

        // Find the minimal steps
        int minSteps = findMinSteps(grid, emptyNode, goalData, maxX, maxY);

        System.out.println("Minimal steps required: " + minSteps);
    }
}