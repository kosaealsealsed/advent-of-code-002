package com.peter_burbery.solve_advent_of_code_with_java_002.year_2023.day_008;

import java.io.*;
import java.util.*;

public class Part_002_Class_002 {
    public static void main(String[] args) {
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\sample-input.txt";

        try {
            // Read the input file
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String line;

            // Parse instructions (first line)
            String instructions = reader.readLine().trim();

            // Parse the network nodes
            Map<String, String[]> nodeMap = new HashMap<>();
            while ((line = reader.readLine()) != null) {
                if (line.contains("=")) {
                    String[] parts = line.split(" = ");
                    String node = parts[0].trim();
                    String[] neighbors = parts[1]
                            .replace("(", "")
                            .replace(")", "")
                            .replace(" ", "")
                            .split(",");

                    // Validate the number of neighbors
                    if (neighbors.length != 2) {
                        System.err.println("Invalid format for node: " + node + ". Expected 2 neighbors but found " + neighbors.length);
                        continue; // Skip invalid node
                    }

                    nodeMap.put(node, neighbors);
                }
            }

            reader.close();

            // Part Two: Simultaneous traversal
            // Identify all starting nodes (nodes ending in A)
            Set<String> currentNodes = new HashSet<>();
            for (String node : nodeMap.keySet()) {
                if (node.endsWith("A")) {
                    currentNodes.add(node);
                }
            }

            // Track the number of steps
            int steps = 0;
            int instructionIndex = 0;

            // Track visited sets to detect cycles
            Set<Set<String>> visitedStates = new HashSet<>();

            // Traverse until all nodes end with Z or a cycle is detected
            while (!allNodesEndWithZ(currentNodes)) {
                // Check for cycles
                if (!visitedStates.add(new HashSet<>(currentNodes))) {
                    System.out.println("Cycle detected at step: " + steps);
                    System.out.println("Current Nodes: " + currentNodes);
                    break;
                }

                // Get the direction (L or R)
                char direction = instructions.charAt(instructionIndex % instructions.length());

                // Create a new set of nodes for the next step
                Set<String> nextNodes = new HashSet<>();
                for (String node : currentNodes) {
                    String[] neighbors = nodeMap.get(node);
                    if (neighbors == null) {
                        throw new IllegalStateException("Node " + node + " has no neighbors defined.");
                    }
                    nextNodes.add(direction == 'L' ? neighbors[0] : neighbors[1]);
                }

                // Move to the next nodes
                currentNodes = nextNodes;
                steps++;
                instructionIndex++;

                // Log progress every 10,000 steps
                if (steps % 10_000 == 0) {
                    System.out.println("Steps: " + steps + ", Current Nodes: " + currentNodes.size());
                    System.out.println("Current Nodes: " + currentNodes);
                }
            }

            // Final output
            if (allNodesEndWithZ(currentNodes)) {
                System.out.println("Steps required to reach all Z nodes: " + steps);
            } else {
                System.out.println("Traversal stopped due to a detected cycle.");
            }
        } catch (IOException e) {
            System.err.println("Error reading the file: " + e.getMessage());
        } catch (Exception e) {
            System.err.println("An error occurred: " + e.getMessage());
        }
    }

    /**
     * Check if all nodes in the set end with Z.
     */
    private static boolean allNodesEndWithZ(Set<String> nodes) {
        for (String node : nodes) {
            if (!node.endsWith("Z")) {
                return false;
            }
        }
        return true;
    }
}
