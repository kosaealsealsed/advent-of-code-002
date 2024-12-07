package com.peter_burbery.solve_advent_of_code_with_java_002.year_2018.day_019;

import java.io.*;
import java.util.*;

public class OpcodeSimulator {

    public static void main(String[] args) throws IOException {
        // Parse the input program file
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2018\\2018-019\\input.txt";
        List<String> lines = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        }

        // Extract the instruction pointer binding and instructions
        int instructionPointerBinding = Integer.parseInt(lines.get(0).split(" ")[1]);
        List<String[]> instructions = new ArrayList<>();
        for (int i = 1; i < lines.size(); i++) {
            instructions.add(lines.get(i).split(" "));
        }

        // Part 1: Execute the program
        int[] registers = new int[6];
        int resultPartOne = executeProgram(registers, instructionPointerBinding, instructions);

        // Print the result of part one
        System.out.println("Part One: " + resultPartOne);

        // Part 2: Reset registers and set register 0 to 1
        registers = new int[6];
        registers[0] = 1;
        int resultPartTwo = executeProgram(registers, instructionPointerBinding, instructions);

        // Print the result of part two
        System.out.println("Part Two: " + resultPartTwo);
    }

    private static int executeProgram(int[] registers, int ipBinding, List<String[]> instructions) {
        int instructionPointer = 0;

        while (instructionPointer >= 0 && instructionPointer < instructions.size()) {
            // Write the instruction pointer to the bound register
            registers[ipBinding] = instructionPointer;

            // Parse the current instruction
            String[] instruction = instructions.get(instructionPointer);
            String opcode = instruction[0];
            int a = Integer.parseInt(instruction[1]);
            int b = Integer.parseInt(instruction[2]);
            int c = Integer.parseInt(instruction[3]);

            // Execute the instruction
            executeOpcode(opcode, a, b, c, registers);

            // Update the instruction pointer from the bound register
            instructionPointer = registers[ipBinding];

            // Move to the next instruction
            instructionPointer++;
        }

        // Return the value of register 0
        return registers[0];
    }

    private static void executeOpcode(String opcode, int a, int b, int c, int[] registers) {
        switch (opcode) {
            case "addr":
                registers[c] = registers[a] + registers[b];
                break;
            case "addi":
                registers[c] = registers[a] + b;
                break;
            case "mulr":
                registers[c] = registers[a] * registers[b];
                break;
            case "muli":
                registers[c] = registers[a] * b;
                break;
            case "banr":
                registers[c] = registers[a] & registers[b];
                break;
            case "bani":
                registers[c] = registers[a] & b;
                break;
            case "borr":
                registers[c] = registers[a] | registers[b];
                break;
            case "bori":
                registers[c] = registers[a] | b;
                break;
            case "setr":
                registers[c] = registers[a];
                break;
            case "seti":
                registers[c] = a;
                break;
            case "gtir":
                registers[c] = (a > registers[b]) ? 1 : 0;
                break;
            case "gtri":
                registers[c] = (registers[a] > b) ? 1 : 0;
                break;
            case "gtrr":
                registers[c] = (registers[a] > registers[b]) ? 1 : 0;
                break;
            case "eqir":
                registers[c] = (a == registers[b]) ? 1 : 0;
                break;
            case "eqri":
                registers[c] = (registers[a] == b) ? 1 : 0;
                break;
            case "eqrr":
                registers[c] = (registers[a] == registers[b]) ? 1 : 0;
                break;
        }
    }
}
