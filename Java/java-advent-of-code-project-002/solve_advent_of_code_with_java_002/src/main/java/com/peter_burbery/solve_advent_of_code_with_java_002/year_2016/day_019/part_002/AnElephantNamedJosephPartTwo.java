package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_019.part_002;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class AnElephantNamedJosephPartTwo {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_019\\input.txt";

        try {
            // Read the input file to get the number of elves
            int numberOfElves = readInput(filePath);

            // Timing start
            long startTime = System.nanoTime();

            // Find the winning elf
            int winningElf = findWinningElfAcrossCircle(numberOfElves);

            // Timing end
            long endTime = System.nanoTime();
            double duration = (endTime - startTime) / 1_000_000_000.0;

            System.out.println("The Elf with all the presents is: " + winningElf);
            System.out.printf("Execution Time: %.9f s%n", duration);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    /**
     * Reads the number of elves from the input file.
     *
     * @param filePath Path to the input file
     * @return Number of elves
     * @throws IOException If there's an error reading the file
     */
    private static int readInput(String filePath) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            return Integer.parseInt(reader.readLine().trim());
        }
    }

    /**
     * Find the winning elf using mathematical optimization.
     *
     * @param n Number of elves
     * @return The position of the winning elf
     */
    private static int findWinningElfAcrossCircle(int n) {
        // Start with 1 elf
        int powerOfThree = 1;

        // Find the largest power of 3 <= n
        while (powerOfThree * 3 <= n) {
            powerOfThree *= 3;
        }

        if (n == powerOfThree) {
            return n; // If n is a power of 3, the answer is n itself.
        } else if (n <= 2 * powerOfThree) {
            return n - powerOfThree; // If n is less than or equal to 2 * largest power of 3
        } else {
            return 2 * n - 3 * powerOfThree; // General case
        }
    }
}
