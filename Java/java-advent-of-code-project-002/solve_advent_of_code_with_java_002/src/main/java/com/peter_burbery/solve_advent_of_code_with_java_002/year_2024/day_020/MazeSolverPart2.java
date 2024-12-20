package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_020;

import java.io.*;
import java.util.*;

public class MazeSolverPart2 {

    static int rows, cols;
    static char[][] grid;
    static int[][] directions = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    public static void main(String[] args) throws IOException {
        String inputFilename = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-020\\input.txt";

        // Read the map from the file
        List<String> lines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(inputFilename))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        }

        rows = lines.size();
        cols = lines.get(0).length();
        grid = new char[rows][cols];

        for (int i = 0; i < rows; i++) {
            grid[i] = lines.get(i).toCharArray();
        }

        // Find S and E
        int[] start = null, end = null;
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                if (grid[r][c] == 'S') {
                    start = new int[]{r, c};
                } else if (grid[r][c] == 'E') {
                    end = new int[]{r, c};
                }
            }
        }

        if (start == null || end == null) {
            System.out.println("Invalid input: Missing start or end point.");
            return;
        }

        // Get distances from S and E
        int[][] distFromS = bfs(start);
        int[][] distFromE = bfs(end);

        int normalDist = distFromS[end[0]][end[1]];
        if (normalDist == Integer.MAX_VALUE) {
            // No normal path from S to E
            System.out.println(0);
            return;
        }

        // Pre-collect all track cells reachable from S
        List<int[]> trackCellsFromS = new ArrayList<>();
        for (int x = 0; x < rows; x++) {
            for (int y = 0; y < cols; y++) {
                if (distFromS[x][y] != Integer.MAX_VALUE && isTrack(grid[x][y])) {
                    trackCellsFromS.add(new int[]{x, y});
                }
            }
        }

        Set<String> cheats = new HashSet<>();

        for (int[] cell : trackCellsFromS) {
            int sx = cell[0], sy = cell[1];

            // BFS ignoring walls from (sx, sy) up to distance 20
            int[][] distIgnoreWalls = new int[rows][cols];
            for (int[] row : distIgnoreWalls) {
                Arrays.fill(row, Integer.MAX_VALUE);
            }

            distIgnoreWalls[sx][sy] = 0;
            Queue<int[]> queue = new LinkedList<>();
            queue.add(new int[]{sx, sy});

            while (!queue.isEmpty()) {
                int[] current = queue.poll();
                int x = current[0], y = current[1];
                int d = distIgnoreWalls[x][y];

                if (d == 20) {
                    continue;
                }

                for (int[] dir : directions) {
                    int nx = x + dir[0], ny = y + dir[1];
                    if (isInBounds(nx, ny) && distIgnoreWalls[nx][ny] > d + 1) {
                        distIgnoreWalls[nx][ny] = d + 1;
                        queue.add(new int[]{nx, ny});
                    }
                }
            }

            // Check all track cells reachable within ≤ 20 steps
            int baseDist = distFromS[sx][sy];
            for (int fx = 0; fx < rows; fx++) {
                for (int fy = 0; fy < cols; fy++) {
                    int d = distIgnoreWalls[fx][fy];
                    if (d != Integer.MAX_VALUE && d >= 1 && d <= 20 && isTrack(grid[fx][fy])) {
                        if (distFromE[fx][fy] != Integer.MAX_VALUE) {
                            int routeWithCheat = baseDist + d + distFromE[fx][fy];
                            int saving = normalDist - routeWithCheat;
                            if (saving >= 100) {
                                cheats.add(sx + "," + sy + "," + fx + "," + fy);
                            }
                        }
                    }
                }
            }
        }

        // Print the count of cheats
        System.out.println(cheats.size());
    }

    static boolean isInBounds(int r, int c) {
        return r >= 0 && r < rows && c >= 0 && c < cols;
    }

    static boolean isTrack(char ch) {
        return ch == '.' || ch == 'S' || ch == 'E';
    }

    static int[][] bfs(int[] start) {
        int[][] dist = new int[rows][cols];
        for (int[] row : dist) {
            Arrays.fill(row, Integer.MAX_VALUE);
        }

        Queue<int[]> queue = new LinkedList<>();
        queue.add(start);
        dist[start[0]][start[1]] = 0;

        while (!queue.isEmpty()) {
            int[] current = queue.poll();
            int x = current[0], y = current[1];

            for (int[] dir : directions) {
                int nx = x + dir[0], ny = y + dir[1];
                if (isInBounds(nx, ny) && isTrack(grid[nx][ny]) && dist[nx][ny] > dist[x][y] + 1) {
                    dist[nx][ny] = dist[x][y] + 1;
                    queue.add(new int[]{nx, ny});
                }
            }
        }

        return dist;
    }
}