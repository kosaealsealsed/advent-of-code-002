package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_022.part_002;

import java.io.*;
import java.nio.file.*;
import java.util.*;

public class Functions {
	public static void main(String[] args) {
        try {
            // Parse the grid from the input file
            String filepath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_022\\input.txt";
            Node[][] grid = parseInput(filepath);

            // Visualize the initial grid
            System.out.println("Initial Grid:");
            visualizeGrid(grid);

            // Find the shortest steps to move goal data to (0,0)
            int steps = findShortestPath(grid);
            System.out.println("Fewest number of steps required: " + steps);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }
	public static Node[][] parseInput(String filepath) throws IOException {
    List<String> lines = Files.readAllLines(Paths.get(filepath));
    List<Node> nodes = new ArrayList<>();

    for (String line : lines) {
        if (!line.startsWith("/dev/grid")) continue; // Skip lines that are not node definitions

        String[] parts = line.split("\\s+"); // Split by whitespace
        if (parts.length < 5) continue; // Ensure we have all parts (Filesystem, Size, Used, Avail, Use%)

        // Extract coordinates (x, y) from the node name
        String[] coords = parts[0].split("-");
        int x = Integer.parseInt(coords[1].substring(1)); // Extract x from "node-xN"
        int y = Integer.parseInt(coords[2].substring(1)); // Extract y from "yN"

        // Parse size and usage values
        int size = Integer.parseInt(parts[1].replace("T", ""));
        int used = Integer.parseInt(parts[2].replace("T", ""));

        nodes.add(new Node(x, y, size, used));
    }

    // Determine grid dimensions
    int maxX = nodes.stream().mapToInt(node -> node.x).max().orElse(0) + 1;
    int maxY = nodes.stream().mapToInt(node -> node.y).max().orElse(0) + 1;

    // Initialize the grid
    Node[][] grid = new Node[maxX][maxY];
    for (Node node : nodes) {
        grid[node.x][node.y] = node;
    }

    return grid;
}
public static void visualizeGrid(Node[][] grid) {
	    for (int y = 0; y < grid[0].length; y++) {
	        for (int x = 0; x < grid.length; x++) {
	            Node node = grid[x][y];
	            if (node.isEmpty()) {
	                System.out.print("_ ");
	            } else if (node.used > 100) { // Arbitrarily mark very large nodes
	                System.out.print("# ");
	            } else if (x == grid.length - 1 && y == 0) { // Goal data
	                System.out.print("G ");
	            } else {
	                System.out.print(". ");
	            }
	        }
	        System.out.println();
	    }
	}
public static int findShortestPath(Node[][] grid) {
    int maxX = grid.length;
    int maxY = grid[0].length;

    // Find empty node
    int emptyX = 0, emptyY = 0;
    for (int y = 0; y < maxY; y++) {
        for (int x = 0; x < maxX; x++) {
            if (grid[x][y].isEmpty()) {
                emptyX = x;
                emptyY = y;
                break;
            }
        }
    }

    // BFS setup
    Queue<State> queue = new LinkedList<>();
    Set<String> visited = new HashSet<>();
    queue.add(new State(emptyX, emptyY, grid));
    visited.add(stateHash(emptyX, emptyY, grid));

    while (!queue.isEmpty()) {
        State current = queue.poll();

        // Check if goal is at (0,0)
        if (current.isGoalAtOrigin()) {
            return current.steps;
        }

        // Explore neighbors
        for (int[] dir : new int[][]{{0, 1}, {1, 0}, {0, -1}, {-1, 0}}) {
            int newX = current.emptyX + dir[0];
            int newY = current.emptyY + dir[1];

            if (isValidMove(newX, newY, maxX, maxY, current.grid)) {
                Node[][] newGrid = current.move(newX, newY);
                String hash = stateHash(newX, newY, newGrid);

                if (!visited.contains(hash)) {
                    queue.add(new State(newX, newY, newGrid, current.steps + 1));
                    visited.add(hash);
                }
            }
        }
    }

    return -1; // No solution found
}
private static boolean isValidMove(int x, int y, int maxX, int maxY, Node[][] grid) {
    return x >= 0 && y >= 0 && x < maxX && y < maxY && grid[x][y].used <= grid[x][y].size;
}

private static String stateHash(int emptyX, int emptyY, Node[][] grid) {
    StringBuilder hash = new StringBuilder();
    hash.append(emptyX).append(",").append(emptyY);
    for (Node[] row : grid) {
        for (Node node : row) {
            hash.append(";").append(node.used);
        }
    }
    return hash.toString();
}



static class State {
    int emptyX, emptyY, steps;
    Node[][] grid;

    State(int emptyX, int emptyY, Node[][] grid) {
        this.emptyX = emptyX;
        this.emptyY = emptyY;
        this.grid = grid;
        this.steps = 0;
    }

    State(int emptyX, int emptyY, Node[][] grid, int steps) {
        this(emptyX, emptyY, grid);
        this.steps = steps;
    }

    boolean isGoalAtOrigin() {
        return grid[0][0].used > 0;
    }

    Node[][] move(int newX, int newY) {
        Node[][] newGrid = deepCopyGrid(grid);
        Node temp = newGrid[newX][newY];
        newGrid[newX][newY] = newGrid[emptyX][emptyY];
        newGrid[emptyX][emptyY] = temp;
        return newGrid;
    }
}

// Helper to deep copy the grid
private static Node[][] deepCopyGrid(Node[][] grid) {
    int maxX = grid.length;
    int maxY = grid[0].length;
    Node[][] newGrid = new Node[maxX][maxY];
    for (int x = 0; x < maxX; x++) {
        for (int y = 0; y < maxY; y++) {
            Node node = grid[x][y];
            newGrid[x][y] = new Node(node.x, node.y, node.size, node.used);
        }
    }
    return newGrid;
}
}
