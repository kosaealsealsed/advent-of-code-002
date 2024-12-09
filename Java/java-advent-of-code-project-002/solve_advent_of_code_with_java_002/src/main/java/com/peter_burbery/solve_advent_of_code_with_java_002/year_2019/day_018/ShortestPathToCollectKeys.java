package com.peter_burbery.solve_advent_of_code_with_java_002.year_2019.day_018;

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class ShortestPathToCollectKeys {

    // Parse the input map and identify keys, doors, and start position
    public static MapData parseMap(String filePath) throws IOException {
        List<String> grid = Files.readAllLines(Paths.get(filePath));
        Map<Character, Position> keys = new HashMap<>();
        Map<Character, Position> doors = new HashMap<>();
        Position start = null;
        char[][] gridArray = new char[grid.size()][];
        
        for (int y = 0; y < grid.size(); y++) {
            gridArray[y] = grid.get(y).toCharArray();
            for (int x = 0; x < gridArray[y].length; x++) {
                char cell = gridArray[y][x];
                if (cell == '@') {
                    start = new Position(x, y);
                } else if (Character.isLowerCase(cell)) {
                    keys.put(cell, new Position(x, y));
                } else if (Character.isUpperCase(cell)) {
                    doors.put(cell, new Position(x, y));
                }
            }
        }
        return new MapData(gridArray, keys, doors, start);
    }

    // Calculate the shortest path to collect all keys using A* heuristic
    public static int shortestPathToCollectKeys(MapData mapData) {
        int allKeysMask = (1 << mapData.keys.size()) - 1; // Bitmask with all keys
        PriorityQueue<State> queue = new PriorityQueue<>(Comparator.comparingInt(s -> s.f));
        Map<Position, Map<Integer, Integer>> visited = new HashMap<>();
        queue.add(new State(mapData.start, 0, 0, heuristic(mapData.start, mapData.keys)));

        while (!queue.isEmpty()) {
            State current = queue.poll();

            if (visited.containsKey(current.position) &&
                visited.get(current.position).getOrDefault(current.keysMask, Integer.MAX_VALUE) <= current.steps) {
                continue;
            }
            visited.computeIfAbsent(current.position, k -> new HashMap<>()).put(current.keysMask, current.steps);

            // Check if all keys are collected
            if (current.keysMask == allKeysMask) {
                return current.steps;
            }

            int x = current.position.x;
            int y = current.position.y;

            for (Position neighbor : getNeighbors(x, y)) {
                char cell = mapData.grid[neighbor.y][neighbor.x];
                if (cell == '#') continue; // Wall

                if (Character.isUpperCase(cell) && ((current.keysMask & (1 << (cell - 'A'))) == 0)) {
                    continue; // Locked door
                }

                int newKeysMask = current.keysMask;
                if (Character.isLowerCase(cell)) {
                    newKeysMask |= (1 << (cell - 'a')); // Collect the key
                }

                int heuristicCost = heuristic(neighbor, mapData.keys);
                queue.add(new State(neighbor, newKeysMask, current.steps + 1, current.steps + 1 + heuristicCost));
            }
        }

        return -1; // No solution
    }

    // Heuristic function for A* (Manhattan distance to the nearest uncollected key)
    public static int heuristic(Position current, Map<Character, Position> keys) {
        int minDistance = Integer.MAX_VALUE;
        for (Position keyPosition : keys.values()) {
            int distance = Math.abs(current.x - keyPosition.x) + Math.abs(current.y - keyPosition.y);
            minDistance = Math.min(minDistance, distance);
        }
        return minDistance;
    }

    // Get neighbors of a given position
    public static List<Position> getNeighbors(int x, int y) {
        return Arrays.asList(
            new Position(x - 1, y),
            new Position(x + 1, y),
            new Position(x, y - 1),
            new Position(x, y + 1)
        );
    }

    // Main method to execute the solution
    public static void main(String[] args) throws IOException {
        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2019\\2019-018\\input.txt"; // Replace with the actual path if needed

        long startTime = System.nanoTime(); // Record start time in nanoseconds

        MapData mapData = parseMap(inputFilePath);
        int result = shortestPathToCollectKeys(mapData);

        long endTime = System.nanoTime(); // Record end time in nanoseconds
        double executionTimeInSeconds = (endTime - startTime) / 1_000_000_000.0; // Convert nanoseconds to seconds

        System.out.println("Shortest path to collect all keys: " + result);
        System.out.printf("Execution time: %.9f s%n", executionTimeInSeconds);
    }

}


