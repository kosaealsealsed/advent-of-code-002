package com.peter_burbery.solve_advent_of_code_with_java_002.year_2019.day_018.part_002;

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class MultiRobotShortestPath {

    // Parse the input map and identify keys, doors, and starting positions for four robots
	public static MapData parseMap(String filePath) throws IOException {
	    List<String> grid = Files.readAllLines(Paths.get(filePath));
	    Map<Character, Position> keys = new HashMap<>();
	    Map<Character, Position> doors = new HashMap<>();
	    List<Position> starts = new ArrayList<>();
	    char[][] gridArray = new char[grid.size()][];

	    for (int y = 0; y < grid.size(); y++) {
	        gridArray[y] = grid.get(y).toCharArray();
	        for (int x = 0; x < gridArray[y].length; x++) {
	            char cell = gridArray[y][x];
	            if (cell == '@') {
	                starts.add(new Position(x, y));
	            } else if (Character.isLowerCase(cell)) {
	                keys.put(cell, new Position(x, y));
	            } else if (Character.isUpperCase(cell)) {
	                doors.put(cell, new Position(x, y));
	            }
	        }
	    }

	    // Validate the map has four starting positions
	    if (starts.size() != 4) {
	        throw new IllegalArgumentException("Map does not contain four starting positions for the robots.");
	    }

	    return new MapData(gridArray, keys, doors, starts);
	}


    // Calculate the shortest path for multiple robots to collect all keys
    public static int shortestPathToCollectKeys(MapData mapData) {
        int allKeysMask = (1 << mapData.keys.size()) - 1; // Bitmask for all keys
        PriorityQueue<State> queue = new PriorityQueue<>(Comparator.comparingInt(State::getHeuristic));
        Map<State, Integer> visited = new HashMap<>();
        queue.add(new State(mapData.starts, 0, 0));

        while (!queue.isEmpty()) {
            State current = queue.poll();

            // Skip if already visited with a shorter or equal path
            if (visited.containsKey(current) && visited.get(current) <= current.steps) {
                continue;
            }
            visited.put(current, current.steps);

            // Check if all keys are collected
            if (current.keysMask == allKeysMask) {
                return current.steps;
            }

            // For each robot, explore neighbors
            for (int i = 0; i < 4; i++) {
                Position robot = current.positions.get(i);
                for (Position neighbor : getNeighbors(robot.x, robot.y)) {
                    char cell = mapData.grid[neighbor.y][neighbor.x];
                    if (cell == '#' || (Character.isUpperCase(cell) && (current.keysMask & (1 << (cell - 'A'))) == 0)) {
                        continue; // Skip walls and locked doors
                    }

                    int newKeysMask = current.keysMask;
                    if (Character.isLowerCase(cell)) {
                        newKeysMask |= (1 << (cell - 'a')); // Collect the key
                    }

                    // Create new state with updated robot position
                    List<Position> newPositions = new ArrayList<>(current.positions);
                    newPositions.set(i, neighbor);
                    State nextState = new State(newPositions, newKeysMask, current.steps + 1);

                    // Add new state to the queue if it's not visited or has a shorter path
                    if (!visited.containsKey(nextState) || visited.get(nextState) > nextState.steps) {
                        queue.add(nextState);
                    }
                }
            }
        }

        return -1; // No solution
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

        long startTime = System.nanoTime(); // Record start time

        MapData mapData = parseMap(inputFilePath);
        int result = shortestPathToCollectKeys(mapData);

        long endTime = System.nanoTime(); // Record end time
        double executionTimeInSeconds = (endTime - startTime) / 1_000_000_000.0; // Convert nanoseconds to seconds

        System.out.println("Shortest path to collect all keys: " + result);
        System.out.printf("Execution time: %.9f s%n", executionTimeInSeconds);
    }
}

// Helper class to represent positions
class Position {
    int x, y;

    Position(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof Position)) return false;
        Position position = (Position) o;
        return x == position.x && y == position.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }
}

// Helper class to represent the state of the search
class State {
    List<Position> positions; // Positions of all robots
    int keysMask;  // Bitmask representing collected keys
    int steps;

    State(List<Position> positions, int keysMask, int steps) {
        this.positions = positions;
        this.keysMask = keysMask;
        this.steps = steps;
    }

    int getHeuristic() {
        return steps; // Heuristic function (could add Manhattan distance or similar here for A*)
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof State)) return false;
        State state = (State) o;
        return keysMask == state.keysMask &&
               Objects.equals(positions, state.positions);
    }

    @Override
    public int hashCode() {
        return Objects.hash(positions, keysMask);
    }
}

// Helper class to store the map data
class MapData {
    char[][] grid;
    Map<Character, Position> keys;
    Map<Character, Position> doors;
    List<Position> starts; // Positions of the four robots

    MapData(char[][] grid, Map<Character, Position> keys, Map<Character, Position> doors, List<Position> starts) {
        this.grid = grid;
        this.keys = keys;
        this.doors = doors;
        this.starts = starts;
    }
}

