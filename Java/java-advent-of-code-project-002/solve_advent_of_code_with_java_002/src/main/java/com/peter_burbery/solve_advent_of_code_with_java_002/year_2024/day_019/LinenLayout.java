package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_019;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class LinenLayout {

    public static void main(String[] args) throws IOException {
        // Parse input
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-019\\input.txt";
        List<String> inputLines = Files.readAllLines(Paths.get(filePath));

        // Separate patterns and designs
        int separatorIndex = inputLines.indexOf("");
        List<String> patterns = List.of(inputLines.get(0).split(", "));
        List<String> designs = new ArrayList<>(inputLines.subList(separatorIndex + 1, inputLines.size()));

        // Part 1: Count possible designs
        int possibleDesignCount = countPossibleDesigns(patterns, designs);
        System.out.println("Part 1: " + possibleDesignCount);

        // Part 2: Total number of arrangements
        long totalArrangementsCount = totalArrangements(patterns, designs);
        System.out.println("Part 2: " + totalArrangementsCount);
    }

    private static int countPossibleDesigns(List<String> patterns, List<String> designs) {
        int possibleCount = 0;
        for (String design : designs) {
            if (isDesignPossible(design, patterns)) {
                possibleCount++;
            }
        }
        return possibleCount;
    }

    private static boolean isDesignPossible(String design, List<String> patterns) {
        int n = design.length();
        boolean[] dp = new boolean[n + 1];
        dp[0] = true; // Base case

        for (int i = 1; i <= n; i++) {
            for (String pattern : patterns) {
                if (i >= pattern.length() && design.substring(i - pattern.length(), i).equals(pattern)) {
                    dp[i] = dp[i] || dp[i - pattern.length()];
                }
            }
        }

        return dp[n];
    }

    private static long totalArrangements(List<String> patterns, List<String> designs) {
        long totalCount = 0;
        for (String design : designs) {
            totalCount += countDesignArrangements(design, patterns);
        }
        return totalCount;
    }

    private static long countDesignArrangements(String design, List<String> patterns) {
        int n = design.length();
        long[] dp = new long[n + 1];
        dp[0] = 1; // Base case

        for (int i = 1; i <= n; i++) {
            for (String pattern : patterns) {
                if (i >= pattern.length() && design.substring(i - pattern.length(), i).equals(pattern)) {
                    dp[i] += dp[i - pattern.length()];
                }
            }
        }

        return dp[n];
    }
}

