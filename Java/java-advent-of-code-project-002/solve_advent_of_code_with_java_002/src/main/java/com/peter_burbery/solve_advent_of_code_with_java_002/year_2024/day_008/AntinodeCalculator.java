package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_008;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

/**
 * Main class for computing antinodes using two methods: pairwise and line-drawing.
 */
public class AntinodeCalculator {

    /**
     * Reads the grid map from a file.
     *
     * @param filename The name of the file containing the grid map.
     * @return A list of strings representing the grid.
     * @throws IOException If there is an error reading the file.
     */
    public static List<String> readMap(String filename) throws IOException {
        return Files.readAllLines(Paths.get(filename));
    }

    /**
     * Computes unique antinode positions using the pairwise method.
     *
     * @param grid A list of strings representing the grid map.
     * @return A set of unique antinode positions.
     */
    public static Set<List<Integer>> computeAntinodesPairwise(List<String> grid) {
        var rows = grid.size();
        var cols = rows > 0 ? grid.get(0).length() : 0;

        // Group antennas by frequency
        Map<Character, List<int[]>> antennasByFreq = new HashMap<>();
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                var ch = grid.get(r).charAt(c);
                if (ch != '.') {
                    antennasByFreq.computeIfAbsent(ch, k -> new ArrayList<>()).add(new int[]{r, c});
                }
            }
        }

        Set<List<Integer>> antinodes = new HashSet<>();

        for (var entry : antennasByFreq.entrySet()) {
            var coords = entry.getValue();
            var n = coords.size();
            if (n < 2) continue;

            for (int i = 0; i < n; i++) {
                var A = coords.get(i);
                var rA = A[0];
                var cA = A[1];
                for (int j = i + 1; j < n; j++) {
                    var B = coords.get(j);
                    var rB = B[0];
                    var cB = B[1];

                    // Compute P1 = 2B - A
                    var p1_r = 2 * rB - rA;
                    var p1_c = 2 * cB - cA;
                    if (p1_r >= 0 && p1_r < rows && p1_c >= 0 && p1_c < cols) {
                        antinodes.add(List.of(p1_r, p1_c));
                    }

                    // Compute P2 = 2A - B
                    var p2_r = 2 * rA - rB;
                    var p2_c = 2 * cA - cB;
                    if (p2_r >= 0 && p2_r < rows && p2_c >= 0 && p2_c < cols) {
                        antinodes.add(List.of(p2_r, p2_c));
                    }
                }
            }
        }
        return antinodes;
    }

    /**
     * Computes unique antinode positions using the line-drawing method.
     *
     * @param grid A list of strings representing the grid map.
     * @return A set of unique antinode positions.
     */
    public static Set<List<Integer>> computeAntinodesLines(List<String> grid) {
        var rows = grid.size();
        var cols = rows > 0 ? grid.get(0).length() : 0;

        // Group antennas by frequency
        Map<Character, List<int[]>> antennasByFreq = new HashMap<>();
        for (int r = 0; r < rows; r++) {
            for (int c = 0; c < cols; c++) {
                var ch = grid.get(r).charAt(c);
                if (ch != '.') {
                    antennasByFreq.computeIfAbsent(ch, k -> new ArrayList<>()).add(new int[]{r, c});
                }
            }
        }

        Set<List<Integer>> antinodes = new HashSet<>();

        for (var entry : antennasByFreq.entrySet()) {
            var coords = entry.getValue();
            var n = coords.size();
            if (n < 2) continue;

            for (int i = 0; i < n; i++) {
                var A = coords.get(i);
                var rA = A[0];
                var cA = A[1];
                for (int j = i + 1; j < n; j++) {
                    var B = coords.get(j);
                    var rB = B[0];
                    var cB = B[1];
                    addLinePoints(rA, cA, rB, cB, rows, cols, antinodes);
                }
            }
        }
        return antinodes;
    }

    /**
     * Adds all points along a line between two antennas to the antinodes set.
     *
     * @param rA       Row of the first antenna.
     * @param cA       Column of the first antenna.
     * @param rB       Row of the second antenna.
     * @param cB       Column of the second antenna.
     * @param rows     Number of rows in the grid.
     * @param cols     Number of columns in the grid.
     * @param antinodes Set to store unique antinode positions.
     */
    private static void addLinePoints(int rA, int cA, int rB, int cB, int rows, int cols, Set<List<Integer>> antinodes) {
        int dr = rB - rA, dc = cB - cA;
        var g = gcd(Math.abs(dr), Math.abs(dc));
        dr /= g;
        dc /= g;

        // Add points in the forward direction
        var rP = rA;
        var cP = cA;
        while (rP >= 0 && rP < rows && cP >= 0 && cP < cols) {
            antinodes.add(List.of(rP, cP));
            rP += dr;
            cP += dc;
        }

        // Add points in the backward direction
        rP = rA - dr;
        cP = cA - dc;
        while (rP >= 0 && rP < rows && cP >= 0 && cP < cols) {
            antinodes.add(List.of(rP, cP));
            rP -= dr;
            cP -= dc;
        }
    }

    /**
     * Computes the greatest common divisor of two integers.
     *
     * @param a First integer.
     * @param b Second integer.
     * @return The greatest common divisor of a and b.
     */
    public static int gcd(int a, int b) {
        return b == 0 ? a : gcd(b, a % b);
    }

    /**
     * Formats the elapsed time in seconds with nanoseconds as a human-readable string.
     *
     * @param nanos The elapsed time in nanoseconds.
     * @return The formatted time as a string.
     */
    private static String formatTime(long nanos) {
        return String.format("%.9f s", nanos / 1e9);
    }

    /**
     * Main function to run the program.
     *
     * @param args Command-line arguments (not used).
     * @throws IOException If there is an error reading the input file.
     */
    public static void main(String[] args) throws IOException {
        var grid = readMap("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-008\\input.txt");

        // Overall timing starts here
        long overallStart = System.nanoTime();

        long part1Time, part2Time;

        // Part 1: Compute using Pairwise method
        var startTime = System.nanoTime();
        var pairwiseAntinodes = computeAntinodesPairwise(grid);
        var endTime = System.nanoTime();
        part1Time = endTime - startTime;
        System.out.println("Part 001 finished in " + formatTime(part1Time));
        System.out.println("Number of unique antinodes (Pairwise method): " + pairwiseAntinodes.size());

        // Part 2: Compute using Line-drawing method
        startTime = System.nanoTime();
        var lineAntinodes = computeAntinodesLines(grid);
        endTime = System.nanoTime();
        part2Time = endTime - startTime;
        System.out.println("Part 002 finished in " + formatTime(part2Time));
        System.out.println("Number of unique antinodes (Line-drawing method): " + lineAntinodes.size());

        // Overall timing ends here
        long overallEnd = System.nanoTime();

        // Compute total computation time (sum of part1Time and part2Time)
        long computationTotalTime = part1Time + part2Time;
        System.out.println("Total computation time: " + formatTime(computationTotalTime));

        // Compute overall runtime (includes overhead)
        long overallTotalTime = overallEnd - overallStart;
        System.out.println("Overall total time: " + formatTime(overallTotalTime));
    }
}
