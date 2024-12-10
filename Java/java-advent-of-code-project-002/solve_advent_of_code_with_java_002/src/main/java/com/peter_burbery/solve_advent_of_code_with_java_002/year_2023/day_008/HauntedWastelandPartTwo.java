package com.peter_burbery.solve_advent_of_code_with_java_002.year_2023.day_008;

//import java.io.*;
//import java.util.*;
//
//public class HauntedWastelandPartTwo {
//
//    public static void main(String[] args) throws IOException {
//        // Replace "input.txt" with the actual file path
//        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
//        Map<String, String[]> network = new HashMap<>();
//        String instructions = "";
//
//        // Parse the input file
//        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
//            instructions = reader.readLine().trim(); // First line contains instructions
//
//            String line;
//            while ((line = reader.readLine()) != null) {
//                line = line.trim();
//                if (!line.isEmpty() && line.contains("=")) {
//                    String[] parts = line.split(" = ");
//                    String node = parts[0].trim();
//                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
//                    network.put(node, connections);
//                }
//            }
//        }
//
//        int steps = navigateGhostlyNetwork(instructions, network);
//        System.out.println("Steps to reach only nodes ending in Z: " + steps);
//    }
//
//    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
//        // Find all nodes ending with 'A'
//        Set<String> startNodes = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("A")) {
//                startNodes.add(node);
//            }
//        }
//
//        int steps = 0;
//        int instructionIndex = 0;
//        int instructionsLength = instructions.length();
//
//        // Keep track of all current nodes
//        Set<String> currentNodes = new HashSet<>(startNodes);
//
//        System.out.println("Starting nodes: " + currentNodes);
//
//        // Continue until all nodes end with 'Z'
//        while (!allNodesEndWithZ(currentNodes)) {
//            Set<String> nextNodes = new HashSet<>();
//
//            char direction = instructions.charAt(instructionIndex);
//            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions
//
//            // Move all current nodes based on the instruction
//            for (String node : currentNodes) {
//                if (network.containsKey(node)) {
//                    if (direction == 'L') {
//                        nextNodes.add(network.get(node)[0]); // Left connection
//                    } else {
//                        nextNodes.add(network.get(node)[1]); // Right connection
//                    }
//                }
//            }
//
//            currentNodes = nextNodes;
//            steps++;
//
//            // Print progress
//            System.out.println("Step " + steps + ":");
//            System.out.println("  Direction: " + direction);
//            System.out.println("  Current nodes: " + currentNodes);
//        }
//
//        return steps;
//    }
//
//    public static boolean allNodesEndWithZ(Set<String> nodes) {
//        for (String node : nodes) {
//            if (!node.endsWith("Z")) {
//                return false;
//            }
//        }
//        return true;
//    }
//}

//import java.io.*;
//import java.util.*;
//
//public class HauntedWastelandPartTwo {
//
//    public static void main(String[] args) throws IOException {
//        // Replace "input.txt" with the actual file path
//        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
//        Map<String, String[]> network = new HashMap<>();
//        String instructions = "";
//
//        // Parse the input file
//        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
//            instructions = reader.readLine().trim(); // First line contains instructions
//
//            String line;
//            while ((line = reader.readLine()) != null) {
//                line = line.trim();
//                if (!line.isEmpty() && line.contains("=")) {
//                    String[] parts = line.split(" = ");
//                    String node = parts[0].trim();
//                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
//                    network.put(node, connections);
//                }
//            }
//        }
//
//        int steps = navigateGhostlyNetwork(instructions, network);
//        System.out.println("Steps to reach only nodes ending in Z: " + steps);
//    }
//
//    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
//        // Precompute nodes ending with Z
//        Set<String> nodesEndingWithZ = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("Z")) {
//                nodesEndingWithZ.add(node);
//            }
//        }
//
//        // Find all starting nodes ending with A
//        Set<String> startNodes = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("A")) {
//                startNodes.add(node);
//            }
//        }
//
//        int steps = 0;
//        int instructionIndex = 0;
//        int instructionsLength = instructions.length();
//
//        // Keep track of all current nodes
//        Set<String> currentNodes = new HashSet<>(startNodes);
//
//        System.out.println("Starting nodes: " + currentNodes);
//
//        // Continue until all nodes end with Z
//        while (!allNodesEndWithZ(currentNodes, nodesEndingWithZ)) {
//            Map<String, Integer> nextNodeCounts = new HashMap<>();
//            char direction = instructions.charAt(instructionIndex);
//            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions
//
//            // Batch process nodes
//            for (String node : currentNodes) {
//                if (network.containsKey(node)) {
//                    String nextNode = (direction == 'L') ? network.get(node)[0] : network.get(node)[1];
//                    nextNodeCounts.put(nextNode, nextNodeCounts.getOrDefault(nextNode, 0) + 1);
//                }
//            }
//
//            currentNodes = nextNodeCounts.keySet();
//            steps++;
//
//            // Print progress
//            System.out.println("Step " + steps + ":");
//            System.out.println("  Direction: " + direction);
//            System.out.println("  Current nodes: " + currentNodes);
//        }
//
//        return steps;
//    }
//
//    public static boolean allNodesEndWithZ(Set<String> nodes, Set<String> nodesEndingWithZ) {
//        return nodesEndingWithZ.containsAll(nodes);
//    }
//}
//


//import java.io.*;
//import java.util.*;
//import java.util.concurrent.ConcurrentHashMap;
//import java.util.stream.Collectors;
//
//public class HauntedWastelandPartTwo {
//
//    public static void main(String[] args) throws IOException {
//        // Replace "input.txt" with the actual file path
//        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
//        Map<String, String[]> network = new HashMap<>();
//        String instructions = "";
//
//        // Parse the input file
//        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
//            instructions = reader.readLine().trim(); // First line contains instructions
//
//            String line;
//            while ((line = reader.readLine()) != null) {
//                line = line.trim();
//                if (!line.isEmpty() && line.contains("=")) {
//                    String[] parts = line.split(" = ");
//                    String node = parts[0].trim();
//                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
//                    network.put(node, connections);
//                }
//            }
//        }
//
//        int steps = navigateGhostlyNetwork(instructions, network);
//        System.out.println("Steps to reach only nodes ending in Z: " + steps);
//    }
//
//    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
//        // Precompute nodes ending with Z
//        Set<String> nodesEndingWithZ = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("Z")) {
//                nodesEndingWithZ.add(node);
//            }
//        }
//
//        // Find all starting nodes ending with A
//        Set<String> startNodes = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("A")) {
//                startNodes.add(node);
//            }
//        }
//
//        int steps = 0;
//        int instructionIndex = 0;
//        int instructionsLength = instructions.length();
//
//        // Keep track of all current nodes and visited states
//        Set<String> currentNodes = new HashSet<>(startNodes);
//        Set<Set<String>> visitedStates = Collections.newSetFromMap(new ConcurrentHashMap<>());
//
//        System.out.println("Starting nodes: " + currentNodes);
//
//        // Continue until all nodes end with Z
//        while (!allNodesEndWithZ(currentNodes, nodesEndingWithZ)) {
//            if (!visitedStates.add(new HashSet<>(currentNodes))) {
//                System.out.println("Detected a cycle. Terminating early.");
//                break;
//            }
//
//            char direction = instructions.charAt(instructionIndex);
//            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions
//
//            // Batch process nodes in parallel
//            Set<String> nextNodes = currentNodes.parallelStream()
//                .filter(node -> network.containsKey(node))
//                .map(node -> (direction == 'L') ? network.get(node)[0] : network.get(node)[1])
//                .collect(Collectors.toSet());
//
//            currentNodes = nextNodes;
//            steps++;
//
//            // Print progress
//            System.out.println("Step " + steps + ":");
//            System.out.println("  Direction: " + direction);
//            System.out.println("  Current nodes: " + currentNodes);
//        }
//
//        return steps;
//    }
//
//    public static boolean allNodesEndWithZ(Set<String> nodes, Set<String> nodesEndingWithZ) {
//        return nodesEndingWithZ.containsAll(nodes);
//    }
//}


//import java.io.*;
//import java.util.*;
//import java.util.concurrent.ConcurrentHashMap;
//import java.util.stream.Collectors;
//
//public class HauntedWastelandPartTwo {
//
//    public static void main(String[] args) throws IOException {
//        // Replace "input.txt" with the actual file path
//        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
//        Map<String, String[]> network = new HashMap<>();
//        String instructions = "";
//
//        // Parse the input file
//        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
//            instructions = reader.readLine().trim(); // First line contains instructions
//
//            String line;
//            while ((line = reader.readLine()) != null) {
//                line = line.trim();
//                if (!line.isEmpty() && line.contains("=")) {
//                    String[] parts = line.split(" = ");
//                    String node = parts[0].trim();
//                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
//                    network.put(node, connections);
//                }
//            }
//        }
//
//        // Compress the instructions
//        String compressedInstructions = compressInstructions(instructions, network.size());
//        System.out.println("Compressed instructions: " + compressedInstructions);
//
//        int steps = navigateGhostlyNetwork(compressedInstructions, network);
//        System.out.println("Steps to reach only nodes ending in Z: " + steps);
//    }
//
//    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
//        // Precompute nodes ending with Z
//        Set<String> nodesEndingWithZ = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("Z")) {
//                nodesEndingWithZ.add(node);
//            }
//        }
//
//        // Find all starting nodes ending with A
//        Set<String> startNodes = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("A")) {
//                startNodes.add(node);
//            }
//        }
//
//        int steps = 0;
//        int instructionIndex = 0;
//        int instructionsLength = instructions.length();
//
//        // Keep track of all current nodes and visited states
//        Set<String> currentNodes = new HashSet<>(startNodes);
//        Set<Set<String>> visitedStates = Collections.newSetFromMap(new ConcurrentHashMap<>());
//
//        System.out.println("Starting nodes: " + currentNodes);
//
//        // Continue until all nodes end with Z
//        while (!allNodesEndWithZ(currentNodes, nodesEndingWithZ)) {
//            if (!visitedStates.add(new HashSet<>(currentNodes))) {
//                System.out.println("Detected a cycle. Terminating early.");
//                break;
//            }
//
//            char direction = instructions.charAt(instructionIndex);
//            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions
//
//            // Batch process nodes in parallel
//            Set<String> nextNodes = currentNodes.parallelStream()
//                .filter(node -> network.containsKey(node))
//                .map(node -> (direction == 'L') ? network.get(node)[0] : network.get(node)[1])
//                .collect(Collectors.toSet());
//
//            currentNodes = nextNodes;
//            steps++;
//
//            // Print progress
//            System.out.println("Step " + steps + ":");
//            System.out.println("  Direction: " + direction);
//            System.out.println("  Current nodes: " + currentNodes);
//        }
//
//        return steps;
//    }
//
//    public static boolean allNodesEndWithZ(Set<String> nodes, Set<String> nodesEndingWithZ) {
//        return nodesEndingWithZ.containsAll(nodes);
//    }
//
//    public static String compressInstructions(String instructions, int networkSize) {
//        // Estimate a sufficient length for instructions to match the traversal
//        int targetLength = networkSize * 2; // Heuristic: twice the network size
//        StringBuilder extendedInstructions = new StringBuilder();
//
//        while (extendedInstructions.length() < targetLength) {
//            extendedInstructions.append(instructions);
//        }
//
//        // Return only the necessary length
//        return extendedInstructions.substring(0, targetLength);
//    }
//}

//import java.io.*;
//import java.util.*;
//import java.util.concurrent.ConcurrentHashMap;
//import java.util.stream.Collectors;
//
//public class HauntedWastelandPartTwo {
//
//    public static void main(String[] args) throws IOException {
//        // Replace "input.txt" with the actual file path
//        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
//        Map<String, String[]> network = new HashMap<>();
//        String instructions = "";
//
//        // Parse the input file
//        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
//            instructions = reader.readLine().trim(); // First line contains instructions
//
//            String line;
//            while ((line = reader.readLine()) != null) {
//                line = line.trim();
//                if (!line.isEmpty() && line.contains("=")) {
//                    String[] parts = line.split(" = ");
//                    String node = parts[0].trim();
//                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
//                    network.put(node, connections);
//                }
//            }
//        }
//
//        // Compress the instructions
//        String compressedInstructions = compressInstructions(instructions, network.size());
//        System.out.println("Compressed instructions: " + compressedInstructions);
//
//        int steps = navigateGhostlyNetwork(compressedInstructions, network);
//        System.out.println("Steps to reach only nodes ending in Z: " + steps);
//    }
//
//    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
//        // Precompute nodes ending with Z
//        Set<String> nodesEndingWithZ = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("Z")) {
//                nodesEndingWithZ.add(node);
//            }
//        }
//
//        // Find all starting nodes ending with A
//        Set<String> startNodes = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("A")) {
//                startNodes.add(node);
//            }
//        }
//
//        int steps = 0;
//        int instructionIndex = 0;
//        int instructionsLength = instructions.length();
//
//        // Keep track of all current nodes and visited states
//        Set<String> currentNodes = new HashSet<>(startNodes);
//        Set<Set<String>> visitedStates = Collections.newSetFromMap(new ConcurrentHashMap<>());
//
//        System.out.println("Starting nodes: " + currentNodes);
//
//        // Continue until all nodes end with Z
//        while (!allNodesEndWithZ(currentNodes, nodesEndingWithZ)) {
//            if (!visitedStates.add(new HashSet<>(currentNodes))) {
//                System.out.println("Detected a cycle. Terminating early.");
//                break;
//            }
//
//            char direction = instructions.charAt(instructionIndex);
//            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions
//
//            // Batch process nodes in parallel
//            Set<String> nextNodes = currentNodes.parallelStream()
//                .filter(node -> network.containsKey(node))
//                .map(node -> (direction == 'L') ? network.get(node)[0] : network.get(node)[1])
//                .collect(Collectors.toSet());
//
//            currentNodes = nextNodes;
//            steps++;
//
//            // Print progress every 10,000 steps
//            if (steps % 100_000 == 0) {
//                System.out.println("Step " + steps + ":");
//                System.out.println("  Current nodes count: " + currentNodes.size());
//            }
//        }
//
//        return steps;
//    }
//
//    public static boolean allNodesEndWithZ(Set<String> nodes, Set<String> nodesEndingWithZ) {
//        return nodesEndingWithZ.containsAll(nodes);
//    }
//
//    public static String compressInstructions(String instructions, int networkSize) {
//        // Estimate a sufficient length for instructions to match the traversal
//        int targetLength = networkSize * 2; // Heuristic: twice the network size
//        StringBuilder extendedInstructions = new StringBuilder();
//
//        while (extendedInstructions.length() < targetLength) {
//            extendedInstructions.append(instructions);
//        }
//
//        // Return only the necessary length
//        return extendedInstructions.substring(0, targetLength);
//    }
//}

//import java.io.*;
//import java.util.*;
//
//public class HauntedWastelandPartTwo {
//
//    public static void main(String[] args) throws IOException {
//        // Replace "input.txt" with the actual file path
//        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
//        Map<String, String[]> network = new HashMap<>();
//        String instructions = "";
//
//        // Parse the input file
//        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
//            instructions = reader.readLine().trim(); // First line contains instructions
//
//            String line;
//            while ((line = reader.readLine()) != null) {
//                line = line.trim();
//                if (!line.isEmpty() && line.contains("=")) {
//                    String[] parts = line.split(" = ");
//                    String node = parts[0].trim();
//                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
//                    network.put(node, connections);
//                }
//            }
//        }
//
//        int steps = navigateGhostlyNetwork(instructions, network);
//        System.out.println("Steps to reach only nodes ending in Z: " + steps);
//    }
//
//    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
//        // Precompute nodes ending with Z
//        Set<String> nodesEndingWithZ = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("Z")) {
//                nodesEndingWithZ.add(node);
//            }
//        }
//
//        // Find all starting nodes ending with A
//        Set<String> startNodes = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("A")) {
//                startNodes.add(node);
//            }
//        }
//
//        int steps = 0;
//        int instructionIndex = 0;
//        int instructionsLength = instructions.length();
//
//        // Keep track of all current nodes
//        Set<String> currentNodes = new HashSet<>(startNodes);
//
//        System.out.println("Starting nodes count: " + currentNodes.size());
//
//        // Continue until all nodes end with Z
//        while (!allNodesEndWithZ(currentNodes, nodesEndingWithZ)) {
//            Map<String, Integer> nextNodeCounts = new HashMap<>();
//            char direction = instructions.charAt(instructionIndex);
//            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions
//
//            // Batch process nodes
//            for (String node : currentNodes) {
//                if (network.containsKey(node)) {
//                    String nextNode = (direction == 'L') ? network.get(node)[0] : network.get(node)[1];
//                    nextNodeCounts.put(nextNode, nextNodeCounts.getOrDefault(nextNode, 0) + 1);
//                }
//            }
//
//            currentNodes = nextNodeCounts.keySet();
//            steps++;
//
//            // Print progress every 100,000 steps
//            if (steps % 10000_000 == 0) {
//                System.out.println("Step " + steps + ":");
//                System.out.println("  Current nodes count: " + currentNodes.size());
//            }
//        }
//
//        return steps;
//    }
//
//    public static boolean allNodesEndWithZ(Set<String> nodes, Set<String> nodesEndingWithZ) {
//        return nodesEndingWithZ.containsAll(nodes);
//    }
//}

//import java.io.*;
//import java.util.*;
//
//public class HauntedWastelandPartTwo {
//
//    public static void main(String[] args) throws IOException {
//        // Replace "input.txt" with the actual file path
//        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
//        Map<String, String[]> network = new HashMap<>();
//        String instructions = "";
//
//        // Parse the input file
//        try (BufferedReader reader = new BufferedReader(new FileReader(inputFilePath))) {
//            instructions = reader.readLine().trim(); // First line contains instructions
//
//            String line;
//            while ((line = reader.readLine()) != null) {
//                line = line.trim();
//                if (!line.isEmpty() && line.contains("=")) {
//                    String[] parts = line.split(" = ");
//                    String node = parts[0].trim();
//                    String[] connections = parts[1].replace("(", "").replace(")", "").split(", ");
//                    network.put(node, connections);
//                }
//            }
//        }
//
//        int steps = navigateGhostlyNetwork(instructions, network);
//        System.out.println("Steps to reach only nodes ending in Z: " + steps);
//    }
//
//    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
//        // Find all starting nodes (ending with A) and target nodes (ending with Z)
//        Set<String> startingNodes = new HashSet<>();
//        Set<String> targetNodes = new HashSet<>();
//        for (String node : network.keySet()) {
//            if (node.endsWith("A")) {
//                startingNodes.add(node);
//            } else if (node.endsWith("Z")) {
//                targetNodes.add(node);
//            }
//        }
//
//        System.out.println("Starting nodes: " + startingNodes.size());
//        System.out.println("Target nodes: " + targetNodes.size());
//
//        Set<String> currentNodes = new HashSet<>(startingNodes);
//        int steps = 0;
//        int instructionIndex = 0;
//        int instructionsLength = instructions.length();
//
//        // Continue until all nodes in the current set end with Z
//        while (!allNodesEndWithZ(currentNodes, targetNodes)) {
//            Set<String> nextNodes = new HashSet<>();
//            char direction = instructions.charAt(instructionIndex);
//            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions
//
//            // Move all nodes based on the direction
//            for (String node : currentNodes) {
//                if (network.containsKey(node)) {
//                    String nextNode = (direction == 'L') ? network.get(node)[0] : network.get(node)[1];
//                    nextNodes.add(nextNode);
//                }
//            }
//
//            currentNodes = nextNodes;
//            steps++;
//
//            // Print progress every 100,000 steps
//            if (steps % 100_000 == 0) {
//                System.out.println("Step " + steps + ": Current nodes count: " + currentNodes.size());
//            }
//        }
//
//        return steps;
//    }
//
//    public static boolean allNodesEndWithZ(Set<String> nodes, Set<String> targetNodes) {
//        return targetNodes.containsAll(nodes);
//    }
//}
import java.io.*;
import java.util.*;

public class HauntedWastelandPartTwo {

    public static void main(String[] args) throws IOException {
        // Replace "input.txt" with the actual file path
        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2023\\2023-008\\input.txt";
        Map<String, String[]> network = new HashMap<>();
        String instructions = "";

        // Parse the input file
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

        int steps = navigateGhostlyNetwork(instructions, network);
        System.out.println("Steps to reach only nodes ending in Z: " + steps);
    }

    public static int navigateGhostlyNetwork(String instructions, Map<String, String[]> network) {
        // Find all starting nodes (ending with A) and target nodes (ending with Z)
        Set<String> startingNodes = new HashSet<>();
        Set<String> targetNodes = new HashSet<>();
        for (String node : network.keySet()) {
            if (node.endsWith("A")) {
                startingNodes.add(node);
            } else if (node.endsWith("Z")) {
                targetNodes.add(node);
            }
        }

        System.out.println("Starting nodes: " + startingNodes);
        System.out.println("Target nodes: " + targetNodes);

        Set<String> currentNodes = new HashSet<>(startingNodes);
        int steps = 0;
        int instructionIndex = 0;
        int instructionsLength = instructions.length();

        // Continue until all nodes in the current set end with Z
        while (!allNodesEndWithZ(currentNodes, targetNodes)) {
            Set<String> nextNodes = new HashSet<>();
            char direction = instructions.charAt(instructionIndex);
            instructionIndex = (instructionIndex + 1) % instructionsLength; // Cycle instructions

            // Move all nodes based on the direction
            for (String node : currentNodes) {
                if (network.containsKey(node)) {
                    String nextNode = (direction == 'L') ? network.get(node)[0] : network.get(node)[1];
                    nextNodes.add(nextNode);
                }
            }

            currentNodes = nextNodes;
            steps++;

            // Print progress every 100,000 steps
            if (steps % 100_000 == 0) {
                System.out.println("Step " + steps + ": Current nodes count: " + currentNodes.size());
            }
        }

        return steps;
    }

    public static boolean allNodesEndWithZ(Set<String> nodes, Set<String> targetNodes) {
        return targetNodes.containsAll(nodes);
    }
}
