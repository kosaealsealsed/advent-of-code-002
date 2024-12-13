package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_011;

import java.io.BufferedReader;
import java.io.FileReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class Main {

    private static List<String> transformStone(String stone) {
        List<String> result = new ArrayList<>();

        if ("0".equals(stone)) {
            result.add("1"); // Rule 1
            return result;
        }

        int length = stone.length();
        if (length % 2 == 0) {
            // Rule 2: Split stone
            int half = length / 2;
            result.add(stripLeadingZeros(stone.substring(0, half)));
            result.add(stripLeadingZeros(stone.substring(half)));
        } else {
            // Rule 3: Multiply by 2024
            BigInteger val = new BigInteger(stone).multiply(BigInteger.valueOf(2024));
            result.add(val.toString());
        }

        return result;
    }

    private static String stripLeadingZeros(String s) {
        return s.replaceFirst("^0+", ""); // Regex to remove leading zeros
    }

    private static List<String> simulateBlinks(List<String> stones, int blinks) {
        for (int i = 0; i < blinks; i++) {
            List<String> newStones = new ArrayList<>();
            for (String stone : stones) {
                newStones.addAll(transformStone(stone));
            }
            stones = newStones;
        }
        return stones;
    }

    public static void main(String[] args) {
        List<String> stones = new ArrayList<>();

        // Read input
        try (BufferedReader br = new BufferedReader(new FileReader("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-011\\input.txt"))) {
            String line = br.readLine().trim();
            String[] parts = line.split("\\s+");
            for (String part : parts) {
                stones.add(part);
            }
        } catch (Exception e) {
            e.printStackTrace();
            return;
        }

        // Simulate 75 blinks
        stones = simulateBlinks(stones, 75);

        // Output the result
        System.out.println(stones.size());
    }
}
