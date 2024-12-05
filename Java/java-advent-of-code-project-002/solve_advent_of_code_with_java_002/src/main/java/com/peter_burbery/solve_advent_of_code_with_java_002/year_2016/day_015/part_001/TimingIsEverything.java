package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_015.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class TimingIsEverything {

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_015\\input.txt";

        try {
            Disc[] discs = parseInput(filePath);
            int time = findFirstValidTime(discs);
            System.out.println("The first time you can press the button is: " + time);
        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static Disc[] parseInput(String filePath) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader(filePath));
        String line;
        Disc[] discs = new Disc[6]; // Adjust the size based on your input
        int index = 0;

        while ((line = reader.readLine()) != null) {
            String[] parts = line.split(" ");
            int positions = Integer.parseInt(parts[3]);
            int startPosition = Integer.parseInt(parts[11].replace(".", ""));
            discs[index++] = new Disc(positions, startPosition);
        }

        reader.close();
        return discs;
    }

    private static int findFirstValidTime(Disc[] discs) {
        int time = 0;

        while (true) {
            boolean allAligned = true;

            for (int i = 0; i < discs.length; i++) {
                if ((discs[i].startPosition + time + i + 1) % discs[i].positions != 0) {
                    allAligned = false;
                    break;
                }
            }

            if (allAligned) {
                return time;
            }

            time++;
        }
    }

    private static class Disc {
        int positions;
        int startPosition;

        Disc(int positions, int startPosition) {
            this.positions = positions;
            this.startPosition = startPosition;
        }
    }
}
