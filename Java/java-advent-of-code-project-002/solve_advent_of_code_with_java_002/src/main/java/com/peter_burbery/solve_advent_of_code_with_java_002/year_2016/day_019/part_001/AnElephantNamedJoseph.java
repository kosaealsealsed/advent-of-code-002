package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_019.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;

public class AnElephantNamedJoseph {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_019\\input.txt";

        try {
            int numberOfElves = readInput(filePath);

            // Timing start
            long startTime = System.nanoTime();

            // Find the winning Elf
            int winningElf = findWinningElf(numberOfElves);

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

    private static int findWinningElf(int numberOfElves) {
        LinkedList<Integer> elves = new LinkedList<>();
        for (int i = 1; i <= numberOfElves; i++) {
            elves.add(i);
        }

        int index = 0;
        while (elves.size() > 1) {
            int nextIndex = (index + 1) % elves.size();
            elves.remove(nextIndex);
            if (nextIndex > index) {
                index = (index + 1) % elves.size();
            }
        }

        return elves.getFirst();
    }
}
