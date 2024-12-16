package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_013;

import org.ejml.data.DMatrixRMaj;
import org.ejml.dense.row.CommonOps_DDRM;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class LinearEquationSolverEJMLRefined {

    // Function to parse input into a list of machines
    public static List<int[]> parseInput(String filePath) throws IOException {
        List<int[]> machines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            List<String> buffer = new ArrayList<>();
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    buffer.add(line);
                    if (buffer.size() == 3) {
                        machines.add(parseMachine(buffer));
                        buffer.clear();
                    }
                }
            }
        }
        return machines;
    }

    // Function to parse a single machine from 3 lines
    private static int[] parseMachine(List<String> lines) {
        String[] buttonA = lines.get(0).replace("Button A: X+", "").replace(" Y+", "").split(",");
        String[] buttonB = lines.get(1).replace("Button B: X+", "").replace(" Y+", "").split(",");
        String[] prize = lines.get(2).replace("Prize: X=", "").replace(" Y=", "").split(",");
        return new int[]{
            Integer.parseInt(buttonA[0]), Integer.parseInt(buttonA[1]),
            Integer.parseInt(buttonB[0]), Integer.parseInt(buttonB[1]),
            Integer.parseInt(prize[0]), Integer.parseInt(prize[1])
        };
    }

    // Function to compute solutions for the machines
    public static int[] computeSolutions(List<int[]> machines, long offsetX, long offsetY) {
        int totalCost = 0;
        int machinesSolved = 0;

        for (int[] machine : machines) {
            int Ax = machine[0], Ay = machine[1];
            int Bx = machine[2], By = machine[3];
            int Px = machine[4] + (int) offsetX, Py = machine[5] + (int) offsetY;

            // Log input data for debugging
            System.out.printf("Machine: Ax=%d, Ay=%d, Bx=%d, By=%d, Px=%d, Py=%d%n", Ax, Ay, Bx, By, Px, Py);

            // Solve the system of equations
            DMatrixRMaj A = new DMatrixRMaj(new double[][]{
                {Ax, Bx},
                {Ay, By}
            });
            DMatrixRMaj B = new DMatrixRMaj(new double[][]{
                {Px},
                {Py}
            });
            DMatrixRMaj X = new DMatrixRMaj(2, 1);

            // Attempt to solve
            try {
                if (!CommonOps_DDRM.solve(A, B, X)) {
                    // Singular matrix or no solution
                    System.out.println("No solution for this machine.");
                    continue;
                }
            } catch (RuntimeException e) {
                System.out.println("Error solving the equations: " + e.getMessage());
                continue;
            }

            // Extract solutions
            double a = X.get(0, 0);
            double b = X.get(1, 0);

            // Log solutions for debugging
            System.out.printf("Solution: a=%.2f, b=%.2f%n", a, b);

            // Check for valid solutions
            if (a >= 0 && b >= 0 && Math.floor(a) == a && Math.floor(b) == b) {
                int intA = (int) a;
                int intB = (int) b;
                int cost = 3 * intA + intB;
                totalCost += cost;
                machinesSolved++;

                // Log cost
                System.out.printf("Valid solution found: a=%d, b=%d, cost=%d%n", intA, intB, cost);
            } else {
                System.out.println("Invalid solution (non-integer or negative).");
            }
        }

        return new int[]{totalCost, machinesSolved};
    }

    // Main function
    public static void main(String[] args) throws IOException {
        String inputFilePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-013\\input.txt"; // Update this path if needed
        List<int[]> machines = parseInput(inputFilePath);

        // Part 1: Solve without offsets
        int[] part1Results = computeSolutions(machines, 0, 0);
        System.out.println("Part 1:");
        System.out.println("Total tokens spent: " + part1Results[0]);
        System.out.println("Machines solved: " + part1Results[1]);

        // Part 2: Solve with large offsets
        long OFFSET = 10_000_000_000_000L;
        int[] part2Results = computeSolutions(machines, OFFSET, OFFSET);
        System.out.println("\nPart 2:");
        System.out.println("Total tokens spent: " + part2Results[0]);
        System.out.println("Machines solved: " + part2Results[1]);
    }
}
