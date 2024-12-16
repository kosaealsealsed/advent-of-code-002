package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_016;

import java.io.*;
import java.util.*;

public class MazeSolver {
    static final int INF = Integer.MAX_VALUE;

    public static List<String> parseInput(String filename) throws IOException {
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line.trim());
            }
        }
        return lines;
    }

    public static int solveMaze(List<String> mazeLines) {
        int[][] directions = {
            {0, 1},   // East
            {1, 0},   // South
            {0, -1},  // West
            {-1, 0}   // North
        };

        int rows = mazeLines.size();
        int cols = mazeLines.get(0).length();
        int[] start = null, end = null;

        // Locate start and end positions
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                char ch = mazeLines.get(r).charAt(c);
                if (ch == 'S') {
                    start = new int[]{r, c};
                } else if (ch == 'E') {
                    end = new int[]{r, c};
                }
            }
        }

        if (start == null || end == null) {
            throw new IllegalArgumentException("Could not find 'S' or 'E' in the maze.");
        }

        int[][][] dist = new int[rows][cols][4];
        for (int[][] layer : dist) {
            for (int[] row : layer) {
                Arrays.fill(row, INF);
            }
        }

        PriorityQueue<int[]> pq = new PriorityQueue<>(Comparator.comparingInt(a -> a[0]));
        dist[start[0]][start[1]][0] = 0;
        pq.offer(new int[]{0, start[0], start[1], 0}); // cost, row, col, direction
        Set<String> visited = new HashSet<>();

        while (!pq.isEmpty()) {
            int[] current = pq.poll();
            int cost = current[0], r = current[1], c = current[2], d = current[3];

            if (r == end[0] && c == end[1]) {
                return cost;
            }

            String stateKey = r + "," + c + "," + d;
            if (visited.contains(stateKey)) continue;
            visited.add(stateKey);

            // Move forward
            int nr = r + directions[d][0];
            int nc = c + directions[d][1];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines.get(nr).charAt(nc) != '#') {
                int newCost = cost + 1;
                if (newCost < dist[nr][nc][d]) {
                    dist[nr][nc][d] = newCost;
                    pq.offer(new int[]{newCost, nr, nc, d});
                }
            }

            // Turn left
            int leftDir = (d + 3) % 4; // equivalent to (d - 1) % 4
            int leftCost = cost + 1000;
            if (leftCost < dist[r][c][leftDir]) {
                dist[r][c][leftDir] = leftCost;
                pq.offer(new int[]{leftCost, r, c, leftDir});
            }

            // Turn right
            int rightDir = (d + 1) % 4;
            int rightCost = cost + 1000;
            if (rightCost < dist[r][c][rightDir]) {
                dist[r][c][rightDir] = rightCost;
                pq.offer(new int[]{rightCost, r, c, rightDir});
            }
        }

        return -1; // No path found
    }

    public static int solvePart2(List<String> mazeLines) {
        int[][] directions = {
            {0, 1},   // East
            {1, 0},   // South
            {0, -1},  // West
            {-1, 0}   // North
        };

        int rows = mazeLines.size();
        int cols = mazeLines.get(0).length();
        int[] start = null, end = null;

        // Locate start and end positions
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                char ch = mazeLines.get(r).charAt(c);
                if (ch == 'S') {
                    start = new int[]{r, c};
                } else if (ch == 'E') {
                    end = new int[]{r, c};
                }
            }
        }

        if (start == null || end == null) {
            throw new IllegalArgumentException("Could not find 'S' or 'E' in the maze.");
        }

        int[][][] dist = new int[rows][cols][4];
        for (int[][] layer : dist) {
            for (int[] row : layer) {
                Arrays.fill(row, INF);
            }
        }

        PriorityQueue<int[]> pq = new PriorityQueue<>(Comparator.comparingInt(a -> a[0]));
        dist[start[0]][start[1]][0] = 0;
        pq.offer(new int[]{0, start[0], start[1], 0}); // cost, row, col, direction
        Set<String> visited = new HashSet<>();

        while (!pq.isEmpty()) {
            int[] current = pq.poll();
            int cost = current[0], r = current[1], c = current[2], d = current[3];

            String stateKey = r + "," + c + "," + d;
            if (visited.contains(stateKey)) continue;
            visited.add(stateKey);

            // Move forward
            int nr = r + directions[d][0];
            int nc = c + directions[d][1];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines.get(nr).charAt(nc) != '#') {
                int newCost = cost + 1;
                if (newCost < dist[nr][nc][d]) {
                    dist[nr][nc][d] = newCost;
                    pq.offer(new int[]{newCost, nr, nc, d});
                }
            }

            // Turn left
            int leftDir = (d + 3) % 4;
            int leftCost = cost + 1000;
            if (leftCost < dist[r][c][leftDir]) {
                dist[r][c][leftDir] = leftCost;
                pq.offer(new int[]{leftCost, r, c, leftDir});
            }

            // Turn right
            int rightDir = (d + 1) % 4;
            int rightCost = cost + 1000;
            if (rightCost < dist[r][c][rightDir]) {
                dist[r][c][rightDir] = rightCost;
                pq.offer(new int[]{rightCost, r, c, rightDir});
            }
        }

        // Trace back the best path
        int minCostEnd = Arrays.stream(dist[end[0]][end[1]]).min().orElse(INF);
        if (minCostEnd == INF) {
            return 0;
        }

        boolean[][] onBestPath = new boolean[rows][cols];
        Queue<int[]> queue = new ArrayDeque<>();
        for (int d = 0; d < 4; d++) {
            if (dist[end[0]][end[1]][d] == minCostEnd) {
                queue.offer(new int[]{end[0], end[1], d});
            }
        }
        Set<String> visitedReverse = new HashSet<>();

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int r = current[0], c = current[1], d = current[2];
            onBestPath[r][c] = true;

            int costHere = dist[r][c][d];

            // Reverse move forward
            int nr = r - directions[d][0];
            int nc = c - directions[d][1];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (mazeLines.get(nr).charAt(nc) != '#' && dist[nr][nc][d] == costHere - 1) {
                    String reverseKey = nr + "," + nc + "," + d;
                    if (!visitedReverse.contains(reverseKey)) {
                        visitedReverse.add(reverseKey);
                        queue.offer(new int[]{nr, nc, d});
                    }
                }
            }

            // Reverse turns
            for (int prevDir : new int[]{(d + 3) % 4, (d + 1) % 4}) {
                if (dist[r][c][prevDir] == costHere - 1000) {
                    String reverseKey = r + "," + c + "," + prevDir;
                    if (!visitedReverse.contains(reverseKey)) {
                        visitedReverse.add(reverseKey);
                        queue.offer(new int[]{r, c, prevDir});
                    }
                }
            }
        }

        int tilesOnBestPath = 0;
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (onBestPath[r][c]) tilesOnBestPath++;
            }
        }

        return tilesOnBestPath;
    }

    public static void main(String[] args) {
        try {
            List<String> mazeLines = parseInput("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-016\\input.txt");

            int part1Result = solveMaze(mazeLines);
            System.out.println("Lowest possible score (Part 1): " + part1Result);

            int part2Result = solvePart2(mazeLines);
            System.out.println("Number of tiles on at least one best path (Part 2): " + part2Result);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
