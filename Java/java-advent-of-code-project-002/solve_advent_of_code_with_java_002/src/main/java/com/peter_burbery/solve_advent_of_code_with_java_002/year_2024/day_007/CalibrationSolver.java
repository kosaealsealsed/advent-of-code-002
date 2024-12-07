package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_007;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;

public class CalibrationSolver {

    // Define a TestCase class to hold target and numbers
    static class TestCase {
        long target;
        List<Long> numbers;

        TestCase(long target, List<Long> numbers) {
            this.target = target;
            this.numbers = numbers;
        }
    }

    // Evaluate expression left to right with + and *
    public static long evaluateLeftToRight(List<Long> numbers, List<String> ops) {
        long result = numbers.get(0);
        for (int i = 0; i < ops.size(); i++) {
            String op = ops.get(i);
            long nextNumber = numbers.get(i + 1);
            switch (op) {
                case "+":
                    result += nextNumber;
                    break;
                case "*":
                    result *= nextNumber;
                    break;
                default:
                    throw new IllegalArgumentException("Unsupported operator: " + op);
            }
        }
        return result;
    }

    // Evaluate expression left to right with +, *, and ||
    public static long evaluateWithConcat(List<Long> numbers, List<String> ops) {
        long result = numbers.get(0);
        for (int i = 0; i < ops.size(); i++) {
            String op = ops.get(i);
            long nextNumber = numbers.get(i + 1);
            switch (op) {
                case "+":
                    result += nextNumber;
                    break;
                case "*":
                    result *= nextNumber;
                    break;
                case "||":
                    // Concatenate the numbers
                    result = Long.parseLong("" + result + nextNumber);
                    break;
                default:
                    throw new IllegalArgumentException("Unsupported operator: " + op);
            }
        }
        return result;
    }

    // Parse input file into a list of TestCase
    public static List<TestCase> parseInput(String filePath) {
        List<TestCase> testCases = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (!line.contains(":")) {
                    continue;
                }
                String[] parts = line.split(":");
                if (parts.length != 2) {
                    continue;
                }
                long target = Long.parseLong(parts[0].trim());
                String[] numStrings = parts[1].trim().split("\\s+");
                List<Long> numbers = new ArrayList<>();
                for (String numStr : numStrings) {
                    numbers.add(Long.parseLong(numStr));
                }
                testCases.add(new TestCase(target, numbers));
            }
        } catch (IOException e) {
            System.err.println("Error reading the input file: " + e.getMessage());
        } catch (NumberFormatException e) {
            System.err.println("Error parsing a number: " + e.getMessage());
        }
        return testCases;
    }

    // Generate all possible operator combinations
    public static List<List<String>> generateOperatorCombinations(List<String> operators, int length) {
        List<List<String>> combinations = new ArrayList<>();
        if (length == 0) {
            combinations.add(new ArrayList<>());
            return combinations;
        }
        List<List<String>> subCombinations = generateOperatorCombinations(operators, length - 1);
        for (List<String> sub : subCombinations) {
            for (String op : operators) {
                List<String> newCombination = new ArrayList<>(sub);
                newCombination.add(op);
                combinations.add(newCombination);
            }
        }
        return combinations;
    }

    // Solve Part One using only + and * operators
    public static long solvePartOne(List<TestCase> testCases) {
        List<String> operators = Arrays.asList("+", "*");
        long validTestValuesSum = 0;
        for (TestCase testCase : testCases) {
            boolean possible = false;
            int opsLength = testCase.numbers.size() - 1;
            List<List<String>> allOps = generateOperatorCombinations(operators, opsLength);
            for (List<String> ops : allOps) {
                long result = evaluateLeftToRight(testCase.numbers, ops);
                if (result == testCase.target) {
                    possible = true;
                    break;
                }
            }
            if (possible) {
                validTestValuesSum += testCase.target;
            }
        }
        return validTestValuesSum;
    }

    // Solve Part Two using +, *, and || operators with progress reporting
    public static Pair<Long, Double> solvePartTwo(List<TestCase> testCases, double partOneTime) {
        List<String> operators = Arrays.asList("+", "*", "||");
        long validTestValuesSum = 0;
        int totalCases = testCases.size();
        int progressInterval = totalCases / 10;
        if (progressInterval == 0) progressInterval = 1; // Handle small number of test cases

        double cumulative1 = 0.0; // Total elapsed time for Part 2 progress
        double cumulative2 = partOneTime; // Cumulative time including Part 1

        System.out.printf("\n%-10s%-20s%-20s%-20s\n", "Progress", "Interval(s)", "Cumulative1(s)", "Cumulative2(s)");
        System.out.println("----------------------------------------------------------------------");

        long startTime = System.nanoTime();
        for (int index = 0; index < totalCases; index++) {
            TestCase testCase = testCases.get(index);
            boolean possible = false;
            int opsLength = testCase.numbers.size() - 1;
            List<List<String>> allOps = generateOperatorCombinations(operators, opsLength);
            for (List<String> ops : allOps) {
                long result = evaluateWithConcat(testCase.numbers, ops);
                if (result == testCase.target) {
                    possible = true;
                    break;
                }
            }
            if (possible) {
                validTestValuesSum += testCase.target;
            }

            // Progress reporting
            if ((index + 1) % progressInterval == 0 || (index + 1) == totalCases) {
                long currentTime = System.nanoTime();
                double intervalElapsed = (currentTime - startTime) / 1_000_000_000.0; // Convert to seconds
                cumulative1 += intervalElapsed;
                cumulative2 = partOneTime + cumulative1;

                System.out.printf("%-10s%-20.9f%-20.9f%-20.9f\n",
                        (index + 1) + "/" + totalCases,
                        intervalElapsed,
                        cumulative1,
                        cumulative2);
                startTime = System.nanoTime(); // Reset timer for next interval
            }
        }

        return new Pair<>(validTestValuesSum, cumulative2);
    }

    // Helper class to return two values
    static class Pair<U, V> {
        public final U first;
        public final V second;

        public Pair(U first, V second) {
            this.first = first;
            this.second = second;
        }
    }

    // Main method
    public static void main(String[] args) {
        // Update filePath with the input file location
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-007\\input.txt"; // Change this to your actual input file

        // Parse the input
        List<TestCase> testCases = parseInput(filePath);

        // Solve Part One
        System.out.println("Starting part 001...");
        long partOneStartTime = System.nanoTime();
        long partOneResult = solvePartOne(testCases);
        double partOneTime = (System.nanoTime() - partOneStartTime) / 1_000_000_000.0; // Convert to seconds
        System.out.printf("Part 1 finished in %.9f s\n", partOneTime);
        System.out.println("Part 1 Total Calibration Result: " + partOneResult);

        // Solve Part Two
        System.out.println("Starting part 002...");
        Pair<Long, Double> partTwo = solvePartTwo(testCases, partOneTime);
        long partTwoResult = partTwo.first;
        double cumulativeTime = partTwo.second;
        double partTwoTime = cumulativeTime - partOneTime;
        System.out.printf("\nPart 2 finished in %.9f s, cumulative time %.9f s\n", partTwoTime, cumulativeTime);
        System.out.println("Part 2 Total Calibration Result: " + partTwoResult);
    }
}
