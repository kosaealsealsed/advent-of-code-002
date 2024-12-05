package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_005;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;

public class UpdateProcessor {

    public static void main(String[] args) {
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-005\\input.txt";
        StringBuilder contentBuilder = new StringBuilder();

        // Reading the file content
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                contentBuilder.append(line).append("\n");
            }
        } catch (IOException e) {
            System.err.println("Error reading the file: " + e.getMessage());
            return;
        }

        String content = contentBuilder.toString().trim();

        // Splitting content into rules and updates
        String[] sections = content.split("\n\n");
        if (sections.length != 2) {
            System.err.println("Invalid input format. Expected two sections separated by two newlines.");
            return;
        }

        String rulesSection = sections[0];
        String updatesSection = sections[1];

        // Parsing rules
        List<int[]> rules = new ArrayList<>();
        for (String ruleLine : rulesSection.split("\\r?\\n")) {
            String[] parts = ruleLine.split("\\|");
            if (parts.length != 2) {
                System.err.println("Invalid rule format: " + ruleLine);
                continue;
            }
            try {
                int x = Integer.parseInt(parts[0].trim());
                int y = Integer.parseInt(parts[1].trim());
                rules.add(new int[]{x, y});
            } catch (NumberFormatException e) {
                System.err.println("Invalid number in rule: " + ruleLine);
            }
        }

        // Parsing updates
        List<List<Integer>> updates = new ArrayList<>();
        for (String updateLine : updatesSection.split("\\r?\\n")) {
            String[] parts = updateLine.split(",");
            List<Integer> update = new ArrayList<>();
            boolean valid = true;
            for (String part : parts) {
                try {
                    update.add(Integer.parseInt(part.trim()));
                } catch (NumberFormatException e) {
                    System.err.println("Invalid number in update: " + updateLine);
                    valid = false;
                    break;
                }
            }
            if (valid) {
                updates.add(update);
            }
        }

        // Identify correctly ordered updates and their middle page numbers
        List<List<Integer>> correctUpdates = new ArrayList<>();
        List<Integer> middlePages = new ArrayList<>();

        for (List<Integer> update : updates) {
            if (isUpdateOrdered(update, rules)) {
                correctUpdates.add(update);
                middlePages.add(getMiddlePage(update));
            }
        }

        // Calculate the sum of middle pages for correct updates
        long sumMiddlePages = middlePages.stream().mapToLong(Integer::longValue).sum();
        System.out.println("Sum of middle pages for correctly ordered updates: " + sumMiddlePages);

        // Identify incorrectly ordered updates, correct them, and collect their middle pages
        List<List<Integer>> incorrectUpdates = new ArrayList<>();
        List<Integer> incorrectMiddlePages = new ArrayList<>();

        for (List<Integer> update : updates) {
            if (!isUpdateOrdered(update, rules)) {
                List<Integer> correctedUpdate = topologicalSortUpdate(update, rules);
                if (correctedUpdate.isEmpty()) {
                    System.err.println("Cycle detected or unable to sort update: " + update);
                    continue;
                }
                incorrectUpdates.add(correctedUpdate);
                incorrectMiddlePages.add(getMiddlePage(correctedUpdate));
            }
        }

        // Calculate the sum of middle pages for corrected updates
        long sumIncorrectMiddlePages = incorrectMiddlePages.stream().mapToLong(Integer::longValue).sum();
        System.out.println("Sum of middle pages for corrected updates: " + sumIncorrectMiddlePages);
    }

    /**
     * Checks if the given update follows all the specified rules.
     *
     * @param update The list of page numbers in the update.
     * @param rules  The list of rules represented as int arrays where rules[i][0] should come before rules[i][1].
     * @return True if the update is correctly ordered according to the rules, false otherwise.
     */
    private static boolean isUpdateOrdered(List<Integer> update, List<int[]> rules) {
        Map<Integer, Integer> indexMap = new HashMap<>();
        for (int i = 0; i < update.size(); i++) {
            indexMap.put(update.get(i), i);
        }

        for (int[] rule : rules) {
            int x = rule[0];
            int y = rule[1];
            if (indexMap.containsKey(x) && indexMap.containsKey(y)) {
                if (indexMap.get(x) > indexMap.get(y)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Performs a topological sort on the given update based on the specified rules.
     *
     * @param update The list of page numbers in the update.
     * @param rules  The list of rules represented as int arrays where rules[i][0] should come before rules[i][1].
     * @return A sorted list of page numbers that adheres to the rules. Returns an empty list if a cycle is detected.
     */
    private static List<Integer> topologicalSortUpdate(List<Integer> update, List<int[]> rules) {
        Map<Integer, List<Integer>> graph = new HashMap<>();
        Map<Integer, Integer> inDegree = new HashMap<>();
        Set<Integer> nodes = new HashSet<>(update);

        // Initialize graph and in-degree
        for (int node : nodes) {
            graph.put(node, new ArrayList<>());
            inDegree.put(node, 0);
        }

        // Build the graph based on rules
        for (int[] rule : rules) {
            int x = rule[0];
            int y = rule[1];
            if (nodes.contains(x) && nodes.contains(y)) {
                graph.get(x).add(y);
                inDegree.put(y, inDegree.get(y) + 1);
            }
        }

        // Initialize the queue with nodes having in-degree 0
        Queue<Integer> queue = new LinkedList<>();
        for (int node : nodes) {
            if (inDegree.get(node) == 0) {
                queue.offer(node);
            }
        }

        List<Integer> sortedUpdate = new ArrayList<>();

        while (!queue.isEmpty()) {
            int current = queue.poll();
            sortedUpdate.add(current);

            for (int neighbor : graph.get(current)) {
                inDegree.put(neighbor, inDegree.get(neighbor) - 1);
                if (inDegree.get(neighbor) == 0) {
                    queue.offer(neighbor);
                }
            }
        }

        // Check if topological sort was possible (i.e., no cycles)
        if (sortedUpdate.size() != nodes.size()) {
            // Cycle detected or unable to sort
            return new ArrayList<>();
        }

        return sortedUpdate;
    }

    /**
     * Retrieves the middle page number from the update list.
     *
     * @param update The list of page numbers.
     * @return The middle page number.
     */
    private static int getMiddlePage(List<Integer> update) {
        return update.get(update.size() / 2);
    }
}
