package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_019.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class AnElephantNamedJosephOptimized {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_019\\input.txt";

        try {
            int numberOfElves = readInput(filePath);

            // Timing start
            long startTime = System.nanoTime();

            // Calculate the winner using Josephus problem
            int winningElf = findWinningElfJosephus(numberOfElves);

            // Timing end
            long endTime = System.nanoTime();
            double duration = (endTime - startTime) / 1_000_000_000.0;

            System.out.println("The Elf with all the presents is: " + winningElf);
            System.out.printf("Execution Time: %.9f s%n", duration);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static int readInput(String filePath) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            return Integer.parseInt(reader.readLine().trim());
        }
    }

    /**
     * Solve Josephus problem for k = 2 using bitwise arithmetic.
     * @param n Number of elves
     * @return Position of the winning elf
     */
    private static int findWinningElfJosephus(int n) {
        // Find the largest power of 2 less than or equal to n
        int highestPowerOf2 = Integer.highestOneBit(n);

        // Calculate the winning position using the formula:
        // Josephus(n) = 2 * (n - highestPowerOf2) + 1
        return 2 * (n - highestPowerOf2) + 1;
    }
}
