package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_021.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ScrambledLettersAndHash {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_021\\input.txt";
        String password = "abcdefgh";

        try {
            long startTime = System.nanoTime();
            String scrambledPassword = scramblePassword(password, filePath);
            long endTime = System.nanoTime();

            double durationInSeconds = (endTime - startTime) / 1_000_000_000.0;
            System.out.println("Scrambled Password: " + scrambledPassword);
            System.out.printf("Execution Time: %.9f s%n", durationInSeconds);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static String scramblePassword(String password, String filePath) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = reader.readLine()) != null) {
                password = applyInstruction(password, line);
            }
        }
        return password;
    }

    private static String applyInstruction(String password, String instruction) {
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
            return rotateLeft(password, steps);
        } else if (instruction.startsWith("rotate right")) {
            int steps = Integer.parseInt(parts[2]);
            return rotateRight(password, steps);
        } else if (instruction.startsWith("rotate based")) {
            char x = parts[6].charAt(0);
            return rotateBasedOnPosition(password, x);
        } else if (instruction.startsWith("reverse positions")) {
            int x = Integer.parseInt(parts[2]);
            int y = Integer.parseInt(parts[4]);
            return reversePositions(password, x, y);
        } else if (instruction.startsWith("move position")) {
            int x = Integer.parseInt(parts[2]);
            int y = Integer.parseInt(parts[5]);
            return movePosition(password, x, y);
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
