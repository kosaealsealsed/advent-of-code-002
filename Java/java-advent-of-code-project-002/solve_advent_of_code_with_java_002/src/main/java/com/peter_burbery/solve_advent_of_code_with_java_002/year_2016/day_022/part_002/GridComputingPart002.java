package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_022.part_002;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class GridComputingPart002 {

    public static void main(String[] args) throws IOException {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_022\\input.txt";

        long startTime = System.nanoTime();

        List<Node> nodes = parseInput(filePath);
        int steps = calculateFewestSteps(nodes);

        long endTime = System.nanoTime();

        System.out.println("Fewest steps required to move goal data to x0-y0: " + steps);
        System.out.printf("Execution Time: %.9f s%n", (endTime - startTime) / 1_000_000_000.0);
    }

    private static class Node {
        int x, y, size, used, avail;

        Node(int x, int y, int size, int used, int avail) {
            this.x = x;
            this.y = y;
            this.size = size;
            this.used = used;
            this.avail = avail;
        }

        boolean isEmpty() {
            return used == 0;
        }

        boolean canMoveDataTo(Node other) {
            return this.used > 0 && this.used <= other.avail;
        }

        @Override
        public String toString() {
            return String.format("Node(x=%d, y=%d, size=%dT, used=%dT, avail=%dT)", x, y, size, used, avail);
        }
    }

    private static List<Node> parseInput(String filePath) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filePath));
        List<Node> nodes = new ArrayList<>();
        String line;

        while ((line = reader.readLine()) != null) {
            if (line.startsWith("/dev/grid")) {
                String[] parts = line.split("\\s+");
                String[] coords = parts[0].split("-");
                int x = Integer.parseInt(coords[1].substring(1));
                int y = Integer.parseInt(coords[2].substring(1));
                int size = Integer.parseInt(parts[1].replace("T", ""));
                int used = Integer.parseInt(parts[2].replace("T", ""));
                int avail = Integer.parseInt(parts[3].replace("T", ""));
                nodes.add(new Node(x, y, size, used, avail));
            }
        }

        reader.close();
        return nodes;
    }

    private static int calculateFewestSteps(List<Node> nodes) {
        // Find grid dimensions
        int maxX = nodes.stream().max(Comparator.comparingInt(node -> node.x)).get().x;
        int maxY = nodes.stream().max(Comparator.comparingInt(node -> node.y)).get().y;

        // Identify special nodes
        Node emptyNode = nodes.stream().filter(Node::isEmpty).findFirst().orElseThrow();
        Node goalNode = nodes.stream().filter(node -> node.x == maxX && node.y == 0).findFirst().orElseThrow();

        // Calculate steps
        // 1. Move empty node next to the goal node
        int stepsToGoal = Math.abs(emptyNode.x - (goalNode.x - 1)) + Math.abs(emptyNode.y - goalNode.y);

        // 2. Move goal data to x0-y0
        int stepsToX0Y0 = (goalNode.x - 1) * 5 + 1;

        return stepsToGoal + stepsToX0Y0;
    }
}
