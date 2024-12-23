package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_022;

import java.io.*;
import java.util.*;

public class SecretNumberSimulation {

    private static final int MOD = 16777216;

    public static int nextSecretNumber(int secret) {
        // Step 1
        int multiplied = secret * 64;
        secret ^= multiplied;
        secret %= MOD;

        // Step 2
        int divided = secret / 32;
        secret ^= divided;
        secret %= MOD;

        // Step 3
        multiplied = secret * 2048;
        secret ^= multiplied;
        secret %= MOD;

        return secret;
    }

    public static int secretAfterNSteps(int initialSecret, int n) {
        int secret = initialSecret;
        for (int i = 0; i < n; i++) {
            secret = nextSecretNumber(secret);
        }
        return secret;
    }

    public static List<List<Integer>> getPriceArrays(List<Integer> buyers) {
        List<List<Integer>> allPrices = new ArrayList<>();

        for (int initialSecret : buyers) {
            List<Integer> prices = new ArrayList<>();
            int secret = initialSecret;
            prices.add(secret % 10); // Add the first price

            for (int i = 0; i < 2000; i++) {
                secret = nextSecretNumber(secret);
                prices.add(secret % 10); // Add subsequent prices
            }
            allPrices.add(prices);
        }

        return allPrices;
    }

    public static int solvePartTwo(String filename) throws IOException {
        // Read buyers' initial secrets from the file
        List<Integer> buyers = new ArrayList<>();

        try (BufferedReader br = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty()) {
                    buyers.add(Integer.parseInt(line));
                }
            }
        }

        // Get the 2001 prices for each buyer
        List<List<Integer>> allPrices = getPriceArrays(buyers);

        Map<List<Integer>, int[]> patternDict = new HashMap<>();

        for (int buyerIndex = 0; buyerIndex < allPrices.size(); buyerIndex++) {
            List<Integer> prices = allPrices.get(buyerIndex);

            for (int i = 0; i < prices.size() - 4; i++) {
                List<Integer> pattern = prices.subList(i, i + 4);
                
                patternDict.putIfAbsent(pattern, new int[buyers.size()]);
                
                if (patternDict.get(pattern)[buyerIndex] == 0) {
                    patternDict.get(pattern)[buyerIndex] = prices.get(i + 4);
                }
            }
        }

        int bestSum = 0;

        for (int[] buyerSells : patternDict.values()) {
            int totalForPattern = Arrays.stream(buyerSells).sum();
            if (totalForPattern > bestSum) {
                bestSum = totalForPattern;
            }
        }

        return bestSum;
    }

    public static void main(String[] args) {
        try {
            int result = solvePartTwo("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-022\\input.txt");
            System.out.println(result);
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }
}

