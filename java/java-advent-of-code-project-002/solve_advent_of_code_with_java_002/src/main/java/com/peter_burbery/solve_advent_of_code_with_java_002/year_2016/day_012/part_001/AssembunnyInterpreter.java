package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_012.part_001;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class AssembunnyInterpreter {

    public static void main(String[] args) throws IOException {
        // Read the input file
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_012\\input.txt";
        List<String> instructions = Files.readAllLines(Paths.get(filePath));

        // Initialize registers
        Map<String, Integer> registers = new HashMap<>();
        registers.put("a", 0);
        registers.put("b", 0);
        registers.put("c", 0);
        registers.put("d", 0);

        // Execute the instructions
        executeInstructions(instructions, registers);

        // Print the result
        System.out.println("Value in register a: " + registers.get("a"));
    }

    private static void executeInstructions(List<String> instructions, Map<String, Integer> registers) {
        int pointer = 0;

        while (pointer < instructions.size()) {
            String[] parts = instructions.get(pointer).split(" ");

            switch (parts[0]) {
                case "cpy":
                    int value = getValue(parts[1], registers);
                    if (registers.containsKey(parts[2])) { // Ensure `y` is a valid register
                        registers.put(parts[2], value);
                    }
                    pointer++;
                    break;

                case "inc":
                    registers.put(parts[1], registers.getOrDefault(parts[1], 0) + 1);
                    pointer++;
                    break;

                case "dec":
                    registers.put(parts[1], registers.getOrDefault(parts[1], 0) - 1);
                    pointer++;
                    break;

                case "jnz":
                    int checkValue = getValue(parts[1], registers);
                    int jumpValue = getValue(parts[2], registers);
                    if (checkValue != 0) {
                        pointer += jumpValue;
                    } else {
                        pointer++;
                    }
                    break;

                default:
                    throw new IllegalArgumentException("Unknown instruction: " + parts[0]);
            }
        }
    }

    private static int getValue(String operand, Map<String, Integer> registers) {
        if (operand.matches("-?\\d+")) { // If operand is an integer
            return Integer.parseInt(operand);
        } else { // If operand is a register
            return registers.getOrDefault(operand, 0);
        }
    }
}

