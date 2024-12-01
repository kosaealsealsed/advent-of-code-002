package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_016.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class DragonChecksum {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_016\\input.txt";
        int diskLength = 272; // Disk size

        try {
            // Read the initial state from the file
            String initialState = readInitialState(filePath);

            // Start timing
            long startTime = System.nanoTime();

            // Calculate checksum
            String checksum = calculateChecksum(initialState, diskLength);

            // End timing
            long endTime = System.nanoTime();

            // Print results
            System.out.println("Checksum: " + checksum);
            System.out.printf("Execution Time: %.9f s%n", (endTime - startTime) / 1_000_000_000.0);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static String readInitialState(String filePath) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            return reader.readLine().trim();
        }
    }

    public static String calculateChecksum(String initialState, int diskLength) {
        // Step 1: Generate data using the dragon curve
        String data = generateDragonData(initialState, diskLength);

        // Step 2: Compute the checksum for the truncated data
        return computeChecksum(data.substring(0, diskLength));
    }

    private static String generateDragonData(String input, int targetLength) {
        StringBuilder data = new StringBuilder(input);

        while (data.length() < targetLength) {
            StringBuilder reversed = new StringBuilder(data).reverse();
            for (int i = 0; i < reversed.length(); i++) {
                reversed.setCharAt(i, reversed.charAt(i) == '0' ? '1' : '0');
            }
            data.append('0').append(reversed);
        }

        return data.toString();
    }

    private static String computeChecksum(String data) {
        StringBuilder checksum = new StringBuilder(data);

        while (checksum.length() % 2 == 0) {
            StringBuilder nextChecksum = new StringBuilder();
            for (int i = 0; i < checksum.length(); i += 2) {
                char a = checksum.charAt(i);
                char b = checksum.charAt(i + 1);
                nextChecksum.append(a == b ? '1' : '0');
            }
            checksum = nextChecksum;
        }

        return checksum.toString();
    }
}
