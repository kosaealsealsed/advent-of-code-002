package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_003;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CorruptedMemoryProcessor {

    // Method to sum all mul(X, Y) operations regardless of do() or don't()
    public static int sumMulOperations(String corruptedMemory) {
        // Regex pattern for mul(X,Y)
        Pattern pattern = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)");
        Matcher matcher = pattern.matcher(corruptedMemory);
        int total = 0;

        // Find all matches and calculate the sum of products
        while (matcher.find()) {
            int num1 = Integer.parseInt(matcher.group(1));
            int num2 = Integer.parseInt(matcher.group(2));
            total += num1 * num2;
        }

        return total;
    }

    // Method to sum only enabled mul(X, Y) operations
    public static int sumEnabledMulOperations(String corruptedMemory) {
        // Regex pattern for do(), don't(), and mul(X,Y)
        Pattern pattern = Pattern.compile("(do\\(\\)|don't\\(\\)|mul\\((\\d{1,3}),(\\d{1,3})\\))");
        Matcher matcher = pattern.matcher(corruptedMemory);

        int totalSum = 0;
        boolean mulEnabled = true; // mul instructions are enabled at the start

        // Iterate over all matches
        while (matcher.find()) {
            String fullMatch = matcher.group(1);

            if (fullMatch.equals("do()")) {
                mulEnabled = true;
            } else if (fullMatch.equals("don't()")) {
                mulEnabled = false;
            } else if (matcher.group(2) != null && matcher.group(3) != null) {
                // This is a mul(X,Y) instruction
                if (mulEnabled) {
                    int num1 = Integer.parseInt(matcher.group(2));
                    int num2 = Integer.parseInt(matcher.group(3));
                    totalSum += num1 * num2;
                }
            }
        }

        return totalSum;
    }

    public static void main(String[] args) {
        try {
            // Read the corrupted memory from 'input.txt'
            String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-003\\input.txt";
            String corruptedMemory = new String(Files.readAllBytes(Paths.get(filePath)));

            // Calculate the results
            int totalSumAllMulOperations = sumMulOperations(corruptedMemory);
            int totalSumEnabledMulOperations = sumEnabledMulOperations(corruptedMemory);

            // Output the results
            System.out.println("The sum of all mul operations is: " + totalSumAllMulOperations);
            System.out.println("The sum of enabled mul operations is: " + totalSumEnabledMulOperations);
        } catch (IOException e) {
            System.err.println("Error reading the file: " + e.getMessage());
        }
    }
}
