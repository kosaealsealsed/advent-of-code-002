package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_010.part_001;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class BalanceBots {

    public static void main(String[] args) throws IOException {
        // Read input file
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\target\\input.txt";
        List<String> lines = Files.readAllLines(Paths.get(filePath));

        // Data structures
        Map<Integer, List<Integer>> bots = new HashMap<>();
        Map<Integer, String[]> rules = new HashMap<>();
        Map<Integer, Integer> outputs = new HashMap<>();

        // Parse input
        for (String line : lines) {
            if (line.startsWith("value")) {
                // Parse "value X goes to bot Y"
                String[] parts = line.split(" ");
                int value = Integer.parseInt(parts[1]);
                int bot = Integer.parseInt(parts[5]);
                bots.computeIfAbsent(bot, k -> new ArrayList<>()).add(value);
            } else if (line.startsWith("bot")) {
                // Parse "bot X gives low to Y and high to Z"
                String[] parts = line.split(" ");
                int bot = Integer.parseInt(parts[1]);
                String lowTarget = parts[5] + " " + parts[6];  // "bot Y" or "output Y"
                String highTarget = parts[10] + " " + parts[11];  // "bot Z" or "output Z"
                rules.put(bot, new String[]{lowTarget, highTarget});
            }
        }

        // Simulate bot operations
        boolean done = false;
        while (!done) {
            done = true;  // Assume we're done unless we process a bot
            for (int bot : new HashSet<>(bots.keySet())) {
                List<Integer> chips = bots.get(bot);
                if (chips.size() == 2) {
                    done = false;  // We found a bot to process

                    // Sort chips to determine low and high
                    Collections.sort(chips);
                    int low = chips.get(0);
                    int high = chips.get(1);

                    // Check for the target comparison
                    if (low == 17 && high == 61) {
                        System.out.println("Bot responsible for comparing 61 and 17: " + bot);
                    }

                    // Apply rules
                    String[] botRules = rules.get(bot);
                    if (botRules != null) {
                        distributeChip(botRules[0], low, bots, outputs);
                        distributeChip(botRules[1], high, bots, outputs);
                    }

                    // Clear chips from this bot
                    bots.get(bot).clear();
                }
            }
        }
    }

    private static void distributeChip(String target, int chip, Map<Integer, List<Integer>> bots, Map<Integer, Integer> outputs) {
        String[] parts = target.split(" ");
        if (parts[0].equals("bot")) {
            int targetBot = Integer.parseInt(parts[1]);
            bots.computeIfAbsent(targetBot, k -> new ArrayList<>()).add(chip);
        } else if (parts[0].equals("output")) {
            int outputBin = Integer.parseInt(parts[1]);
            outputs.put(outputBin, chip);
        }
    }
}
