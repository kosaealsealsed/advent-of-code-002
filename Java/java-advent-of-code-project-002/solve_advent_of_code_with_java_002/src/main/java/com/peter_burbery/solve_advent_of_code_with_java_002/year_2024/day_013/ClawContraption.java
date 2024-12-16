package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_013;

import java.io.*;
import java.util.*;
import org.apache.commons.math3.linear.*;

public class ClawContraption {

    static class Button {
        int dx, dy, cost;

        Button(int dx, int dy, int cost) {
            this.dx = dx;
            this.dy = dy;
            this.cost = cost;
        }
    }

    static class Prize {
        int x, y;

        Prize(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }

    static class Machine {
        Button buttonA;
        Button buttonB;
        Prize prize;

        Machine(Button buttonA, Button buttonB, Prize prize) {
            this.buttonA = buttonA;
            this.buttonB = buttonB;
            this.prize = prize;
        }
    }

    static class Result {
        boolean solvable;
        int cost;

        Result(boolean solvable, int cost) {
            this.solvable = solvable;
            this.cost = cost;
        }
    }

    public static void main(String[] args) throws IOException {
        String filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-013\\input.txt";
        List<Machine> machines = parseInput(filePath);

        int totalCost = 0;
        int prizesWon = 0;

        for (Machine machine : machines) {
            Result result = solveMachine(machine);
            if (result.solvable) {
                prizesWon++;
                totalCost += result.cost;
            }
        }

        System.out.println("Total prizes won: " + prizesWon);
        System.out.println("Total cost: " + totalCost);
    }

    private static List<Machine> parseInput(String filePath) throws IOException {
        List<Machine> machines = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (line.startsWith("Button A")) {
                    int ax = Integer.parseInt(line.split("X\\+")[1].split(",")[0].trim());
                    int ay = Integer.parseInt(line.split("Y\\+")[1].trim());
                    int bx, by, px, py;

                    line = br.readLine();
                    bx = Integer.parseInt(line.split("X\\+")[1].split(",")[0].trim());
                    by = Integer.parseInt(line.split("Y\\+")[1].trim());

                    line = br.readLine();
                    px = Integer.parseInt(line.split("X=")[1].split(",")[0].trim());
                    py = Integer.parseInt(line.split("Y=")[1].trim());

                    Button buttonA = new Button(ax, ay, 3);
                    Button buttonB = new Button(bx, by, 1);
                    Prize prize = new Prize(px, py);

                    machines.add(new Machine(buttonA, buttonB, prize));
                }
            }
        }
        return machines;
    }

    private static Result solveMachine(Machine machine) {
        Button a = machine.buttonA;
        Button b = machine.buttonB;
        Prize p = machine.prize;

        RealMatrix coefficients = new Array2DRowRealMatrix(new double[][] {
            {a.dx, b.dx},
            {a.dy, b.dy}
        }, false);

        DecompositionSolver solver = new LUDecomposition(coefficients).getSolver();

        if (!solver.isNonSingular()) {
            return new Result(false, 0);
        }

        RealVector constants = new ArrayRealVector(new double[] {p.x, p.y}, false);
        RealVector solution = solver.solve(constants);

        double x = solution.getEntry(0);
        double y = solution.getEntry(1);

        if (x < 0 || y < 0 || x != Math.floor(x) || y != Math.floor(y)) {
            return new Result(false, 0);
        }

        int pressesA = (int) x;
        int pressesB = (int) y;

        int totalCost = pressesA * a.cost + pressesB * b.cost;
        return new Result(true, totalCost);
    }
}
