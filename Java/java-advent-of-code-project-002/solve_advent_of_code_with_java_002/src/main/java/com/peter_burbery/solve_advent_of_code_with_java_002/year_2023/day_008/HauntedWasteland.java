package com.peter_burbery.solve_advent_of_code_with_java_002.year_2023.day_008;

import java.io.*;
import java.util.*;

public class HauntedWasteland {

    public static void main(String[] args) throws IOException {
        // Replace "input.txt" with the actual file path
        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
        Map<String, String[]> network = new HashMap<>();
        String instructions = "";

        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
            instructions = reader.readLine().trim(); // First line contains instructions

            String line;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty() && line.contains("=")) {
                    String[] parts = line.split(" = ");
                    String node = parts[0].trim();
                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
                    network.put(node, connections);
                }
            }
        }

        int steps = navigateNetwork(instructions, network);
        System.out.println("Steps to reach ZZZ: " + steps);
    }

    public static int navigateNetwork(String instructions, Map<String, String[]> network) {
        String currentNode = "AAA"; // Starting node
        int steps = 0;
        int instructionIndex = 0;
        int instructionsLength = instructions.length();

        while (!currentNode.equals("ZZZ")) {
            char direction = instructions.charAt(instructionIndex);
            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions

            // Navigate based on the direction
            if (direction == 'L') {
                currentNode = network.get(currentNode)[0]; // Left connection
            } else {
                currentNode = network.get(currentNode)[1]; // Right connection
            }
            steps++;
        }

        return steps;
    }
}

