package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_022.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class GridComputingPart001 {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_022\\input.txt";

        try {
            List<Node> nodes = parseInput(filePath);
            long startTime = System.nanoTime();
            int viablePairs = countViablePairs(nodes);
            long endTime = System.nanoTime();

            double durationInSeconds = (endTime - startTime) / 1_000_000_000.0;
            System.out.println("Number of viable pairs: " + viablePairs);
            System.out.printf("Execution Time: %.9f s%n", durationInSeconds);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static List<Node> parseInput(String filePath) throws IOException {
        List<Node> nodes = new ArrayList<>();
        BufferedReader reader = new BufferedReader(new FileReader(filePath));

        // Skip the first two lines of the file
        reader.readLine();
        reader.readLine();

        String line;
        while ((line = reader.readLine()) != null) {
            String[] parts = line.split("\\s+");
            String[] position = parts[0].split("-");
            int x = Integer.parseInt(position[1].substring(1));
            int y = Integer.parseInt(position[2].substring(1));
            int size = Integer.parseInt(parts[1].replace("T", ""));
            int used = Integer.parseInt(parts[2].replace("T", ""));
            int avail = Integer.parseInt(parts[3].replace("T", ""));
            nodes.add(new Node(x, y, size, used, avail));
        }

        reader.close();
        return nodes;
    }

    private static int countViablePairs(List<Node> nodes) {
        int count = 0;

        for (int i = 0; i < nodes.size(); i++) {
            Node a = nodes.get(i);
            if (a.used == 0) continue; // Node A must not be empty

            for (int j = 0; j < nodes.size(); j++) {
                if (i == j) continue; // A and B must be different nodes
                Node b = nodes.get(j);
                if (a.used <= b.avail) {
                    count++;
                }
            }
        }

        return count;
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
    }
}
