package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_002;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;

public class SafeReports {

    // Method to check if a report is safe
    public static boolean isSafe(List<Integer> report) {
        List<Integer> differences = IntStream.range(0, report.size() - 1)
            .map(i -> report.get(i + 1) - report.get(i))
            .boxed()
            .collect(Collectors.toList());

        boolean allIncreasing = differences.stream().allMatch(diff -> diff >= 1 && diff <= 3);
        boolean allDecreasing = differences.stream().allMatch(diff -> diff >= -3 && diff <= -1);

        return allIncreasing || allDecreasing;
    }

    // Method to check if a report is safe with the Problem Dampener
    public static boolean isSafeWithDampener(List<Integer> report) {
        if (isSafe(report)) {
            return true;
        }

        for (int i = 0; i < report.size(); i++) {
            List<Integer> modifiedReport = new ArrayList<>(report);
            modifiedReport.remove(i);
            if (isSafe(modifiedReport)) {
                return true;
            }
        }
        return false;
    }

    public static void main(String[] args) throws IOException {
        // Input file path
        String inputPath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-002\\input.txt";

        // Read input lines from the file
        List<String> lines = Files.readAllLines(Paths.get(inputPath));
        List<List<Integer>> reports = lines.stream()
            .map(line -> Arrays.stream(line.split("\\s+"))
                               .map(Integer::parseInt)
                               .collect(Collectors.toList()))
            .collect(Collectors.toList());

        // Count safe reports
        long safeCount = reports.stream().filter(SafeReports::isSafe).count();
        System.out.println("Safe reports: " + safeCount);

        // Count safe reports with the Problem Dampener
        long safeWithDampenerCount = reports.stream().filter(SafeReports::isSafeWithDampener).count();
        System.out.println("Safe reports with dampener: " + safeWithDampenerCount);
    }
}
