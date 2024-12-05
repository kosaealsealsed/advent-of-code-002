package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_021.part_002;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class UnscramblePassword {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_021\\input.txt";
        String scrambledPassword = "fbgdceah";

        try {
            long startTime = System.nanoTime();
            String unscrambledPassword = unscramblePassword(scrambledPassword, filePath);
            long endTime = System.nanoTime();

            double durationInSeconds = (endTime - startTime) / 1_000_000_000.0;
            System.out.println("Unscrambled Password: " + unscrambledPassword);
            System.out.printf("Execution Time: %.9f s%n", durationInSeconds);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static String unscramblePassword(String scrambledPassword, String filePath) throws IOException {
        List<String> instructions;
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            instructions = new ArrayList<>(reader.lines().toList());
        }
        // Convert to mutable list and reverse the instructions
        Collections.reverse(instructions);

        for (String instruction : instructions) {
            scrambledPassword = reverseInstruction(scrambledPassword, instruction);
        }
        return scrambledPassword;
    }

    private static String reverseInstruction(String password, String instruction) {
        String[] parts = instruction.split(" ");
        if (instruction.startsWith("swap position")) {
            int x = Integer.parseInt(parts[2]);
            int y = Integer.parseInt(parts[5]);
            return swapPosition(password, x, y);
        } else if (instruction.startsWith("swap letter")) {
            char x = parts[2].charAt(0);
            char y = parts[5].charAt(0);
            return swapLetter(password, x, y);
        } else if (instruction.startsWith("rotate left")) {
            int steps = Integer.parseInt(parts[2]);
            return rotateRight(password, steps);
        } else if (instruction.startsWith("rotate right")) {
            int steps = Integer.parseInt(parts[2]);
            return rotateLeft(password, steps);
        } else if (instruction.startsWith("rotate based")) {
            char x = parts[6].charAt(0);
            return reverseRotateBasedOnPosition(password, x);
        } else if (instruction.startsWith("reverse positions")) {
            int x = Integer.parseInt(parts[2]);
            int y = Integer.parseInt(parts[4]);
            return reversePositions(password, x, y);
        } else if (instruction.startsWith("move position")) {
            int x = Integer.parseInt(parts[2]);
            int y = Integer.parseInt(parts[5]);
            return movePosition(password, y, x);
        }
        return password; // No valid instruction
    }

    private static String swapPosition(String password, int x, int y) {
        char[] chars = password.toCharArray();
        char temp = chars[x];
        chars[x] = chars[y];
        chars[y] = temp;
        return new String(chars);
    }

    private static String swapLetter(String password, char x, char y) {
        char[] chars = password.toCharArray();
        for (int i = 0; i < chars.length; i++) {
            if (chars[i] == x) {
                chars[i] = y;
            } else if (chars[i] == y) {
                chars[i] = x;
            }
        }
        return new String(chars);
    }

    private static String rotateLeft(String password, int steps) {
        steps = steps % password.length();
        return password.substring(steps) + password.substring(0, steps);
    }

    private static String rotateRight(String password, int steps) {
        steps = steps % password.length();
        return password.substring(password.length() - steps) + password.substring(0, password.length() - steps);
    }

    private static String reverseRotateBasedOnPosition(String password, char x) {
        int len = password.length();
        for (int i = 0; i < len; i++) {
            String rotated = rotateLeft(password, i);
            if (rotateBasedOnPosition(rotated, x).equals(password)) {
                return rotated;
            }
        }
        throw new IllegalStateException("Reverse rotation failed");
    }

    private static String rotateBasedOnPosition(String password, char x) {
        int index = password.indexOf(x);
        int steps = 1 + index;
        if (index >= 4) {
            steps++;
        }
        return rotateRight(password, steps);
    }

    private static String reversePositions(String password, int x, int y) {
        char[] chars = password.toCharArray();
        while (x < y) {
            char temp = chars[x];
            chars[x] = chars[y];
            chars[y] = temp;
            x++;
            y--;
        }
        return new String(chars);
    }

    private static String movePosition(String password, int x, int y) {
        char[] chars = password.toCharArray();
        char toMove = chars[x];
        if (x < y) {
            System.arraycopy(chars, x + 1, chars, x, y - x);
        } else {
            System.arraycopy(chars, y, chars, y + 1, x - y);
        }
        chars[y] = toMove;
        return new String(chars);
    }
}
