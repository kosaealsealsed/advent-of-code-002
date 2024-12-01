package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_013.part_001;

import java.util.*;

public class MazeSolver {

    public static void main(String[] args) {
        int favoriteNumber = 1352; // Your puzzle input
        int targetX = 31;
        int targetY = 39;

        // Solve the maze
        int steps = findShortestPath(favoriteNumber, targetX, targetY);
        System.out.println("Fewest number of steps to reach (" + targetX + "," + targetY + "): " + steps);
    }

    public static int findShortestPath(int favoriteNumber, int targetX, int targetY) {
        // BFS setup
        Queue<State> queue = new LinkedList<>();
        Set<String> visited = new HashSet<>();

        // Starting point
        queue.add(new State(1, 1, 0)); // Start at (1, 1) with 0 steps
        visited.add("1,1");

        while (!queue.isEmpty()) {
            State current = queue.poll();

            // Check if we've reached the target
            if (current.x == targetX && current.y == targetY) {
                return current.steps;
            }

            // Explore neighbors
            for (int[] direction : new int[][]{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}) {
                int newX = current.x + direction[0];
                int newY = current.y + direction[1];
                String key = newX + "," + newY;

                // Check bounds and wall conditions
                if (newX >= 0 && newY >= 0 && !visited.contains(key) && isOpenSpace(newX, newY, favoriteNumber)) {
                    queue.add(new State(newX, newY, current.steps + 1));
                    visited.add(key);
                }
            }
        }

        return -1; // No path found (shouldn't happen in this problem)
    }

    public static boolean isOpenSpace(int x, int y, int favoriteNumber) {
        int value = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
        int bits = Integer.bitCount(value);
        return bits % 2 == 0; // Even number of bits -> open space
    }

    private static class State {
        int x, y, steps;

        public State(int x, int y, int steps) {
            this.x = x;
            this.y = y;
            this.steps = steps;
        }
    }
}

