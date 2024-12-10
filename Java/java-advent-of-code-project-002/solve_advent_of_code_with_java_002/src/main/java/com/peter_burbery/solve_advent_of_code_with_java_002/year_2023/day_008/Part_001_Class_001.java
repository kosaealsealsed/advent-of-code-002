package com.peter_burbery.solve_advent_of_code_with_java_002.year_2023.day_008;

import java.io.*;
import java.util.*;

public class Part_001_Class_001 {
    public static void main(String[] args) {
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";

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

            // Start traversal
            String currentNode = "AAA"; // Starting point
            int steps = 0;
            int instructionIndex = 0;

            while (!currentNode.equals("ZZZ")) {
                // Get the direction (L or R)
                char direction = instructions.charAt(instructionIndex % instructions.length());

                // Traverse to the next node based on the direction
                String[] neighbors = nodeMap.get(currentNode);
                if (neighbors == null) {
                    throw new IllegalStateException("Node " + currentNode + " has no neighbors defined.");
                }

                currentNode = direction == 'L' ? neighbors[0] : neighbors[1];
                steps++;

                // Move to the next instruction
                instructionIndex++;
            }

            System.out.println("Steps required to reach ZZZ: " + steps);
        } catch (IOException e) {
            System.err.println("Error reading the file: " + e.getMessage());
        } catch (Exception e) {
            System.err.println("An error occurred: " + e.getMessage());
        }
    }
}


