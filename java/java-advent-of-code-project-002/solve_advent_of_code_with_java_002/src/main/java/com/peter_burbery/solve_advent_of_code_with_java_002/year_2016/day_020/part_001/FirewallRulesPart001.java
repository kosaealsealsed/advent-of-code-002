package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_020.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class FirewallRulesPart001 {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_020\\input.txt";

        try {
            List<Range> blockedRanges = parseInput(filePath);
            long lowestAllowedIP = findLowestAllowedIP(blockedRanges);
            System.out.println("The lowest-valued IP that is not blocked: " + lowestAllowedIP);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    /**
     * Parse the input file to extract the list of blocked IP ranges.
     *
     * @param filePath Path to the input file
     * @return A list of blocked IP ranges
     * @throws IOException If there's an error reading the file
     */
    private static List<Range> parseInput(String filePath) throws IOException {
        List<Range> ranges = new ArrayList<>();

        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split("-");
                long start = Long.parseLong(parts[0]);
                long end = Long.parseLong(parts[1]);
                ranges.add(new Range(start, end));
            }
        }

        return ranges;
    }

    /**
     * Find the lowest-valued IP that is not blocked.
     *
     * @param blockedRanges List of blocked IP ranges
     * @return The lowest-valued IP that is not blocked
     */
    private static long findLowestAllowedIP(List<Range> blockedRanges) {
        // Sort ranges by start, and then by end if starts are equal
        Collections.sort(blockedRanges, (a, b) -> {
            if (a.start != b.start) {
                return Long.compare(a.start, b.start);
            }
            return Long.compare(a.end, b.end);
        });

        long lowestAllowed = 0;

        for (Range range : blockedRanges) {
            if (lowestAllowed < range.start) {
                break;
            }
            lowestAllowed = Math.max(lowestAllowed, range.end + 1);
        }

        return lowestAllowed;
    }

    /**
     * Helper class to represent a range of blocked IPs.
     */
    private static class Range {
        long start;
        long end;

        Range(long start, long end) {
            this.start = start;
            this.end = end;
        }
    }
}
