package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_020.part_002;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class FirewallRulesPart002 {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_020\\input.txt";

        try {
            List<Range> blockedRanges = parseInput(filePath);
            long totalAllowedIPs = calculateTotalAllowedIPs(blockedRanges, 4294967295L);
            System.out.println("Total number of allowed IPs: " + totalAllowedIPs);
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
     * Calculate the total number of allowed IPs based on the blocked ranges.
     *
     * @param blockedRanges List of blocked IP ranges
     * @param maxIP         Maximum IP value (e.g., 4294967295 for 32-bit integers)
     * @return Total number of allowed IPs
     */
    private static long calculateTotalAllowedIPs(List<Range> blockedRanges, long maxIP) {
        // Sort ranges by start, and then by end if starts are equal
        Collections.sort(blockedRanges, (a, b) -> {
            if (a.start != b.start) {
                return Long.compare(a.start, b.start);
            }
            return Long.compare(a.end, b.end);
        });

        long allowedIPs = 0;
        long currentIP = 0;

        for (Range range : blockedRanges) {
            if (currentIP < range.start) {
                // Count the gap between current IP and the start of this range
                allowedIPs += range.start - currentIP;
            }
            // Move current IP past this range's end
            currentIP = Math.max(currentIP, range.end + 1);
        }

        // Add any remaining IPs after the last range
        if (currentIP <= maxIP) {
            allowedIPs += maxIP - currentIP + 1;
        }

        return allowedIPs;
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
