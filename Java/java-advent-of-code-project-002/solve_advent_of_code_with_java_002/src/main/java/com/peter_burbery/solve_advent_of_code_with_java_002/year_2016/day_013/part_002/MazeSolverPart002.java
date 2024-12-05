package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_013.part_002;

import java.util.*;

public class MazeSolverPart002 {

    public static void main(String[] args) {
        int favoriteNumber = 1352; // Your puzzle input
        int maxSteps = 50;

        // Find the number of reachable locations within 50 steps
        int reachableLocations = countReachableLocations(favoriteNumber, maxSteps);
        System.out.println("Number of locations reachable in at most 50 steps: " + reachableLocations);
    }

    public static int countReachableLocations(int favoriteNumber, int maxSteps) {
        // BFS setup
        Queue<State> queue = new LinkedList<>();
        Set<String> visited = new HashSet<>();

        // Starting point
        queue.add(new State(1, 1, 0)); // Start at (1, 1) with 0 steps
        visited.add("1,1");

        while (!queue.isEmpty()) {
            State current = queue.poll();

            // Stop exploring further if we've exceeded the step limit
            if (current.steps > maxSteps) {
                continue;
            }

            // Explore neighbors
            for (int[] direction : new int[][]{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}) {
                int newX = current.x + direction[0];
                int newY = current.y + direction[1];
                String key = newX + "," + newY;

                // Check bounds and wall conditions
                if (newX >= 0 && newY >= 0 && !visited.contains(key) && isOpenSpace(newX, newY, favoriteNumber)) {
                    // Only add to the queue and mark as visited if within the step limit
                    if (current.steps + 1 <= maxSteps) {
                        queue.add(new State(newX, newY, current.steps + 1));
                        visited.add(key); // Mark as visited here
                    }
                }
            }
        }

        // Return the count of visited locations
        return visited.size();
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


