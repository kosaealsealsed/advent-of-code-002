package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_014.part_002;

import pt.kcry.blake3.Blake3;
import java.util.LinkedList;
import java.util.Queue;

public class OneTimePadKeyStretchingBlake3 {

    public static void main(String[] args) {
        String salt = "zpqevtbw"; // Replace with your puzzle input
        int targetKeyIndex = 64;

        // Timing for Blake3
        long startTimeBlake3 = System.nanoTime();
        int indexBlake3 = find64thKeyIndexWithBlake3Stretching(salt, targetKeyIndex);
        long endTimeBlake3 = System.nanoTime();
        double durationBlake3 = (endTimeBlake3 - startTimeBlake3) / 1_000_000_000.0;
        System.out.printf("Blake3: Index producing the 64th key: %d in %.9f s%n", indexBlake3, durationBlake3);
    }

    public static int find64thKeyIndexWithBlake3Stretching(String salt, int targetKeyIndex) {
        Queue<String> futureHashes = new LinkedList<>();
        int keysFound = 0;
        int index = 0;

        // Precompute the first 1000 stretched hashes
        for (int i = 0; i < 1000; i++) {
            futureHashes.add(generateStretchedHashBlake3(salt + i));
        }

        while (keysFound < targetKeyIndex) {
            // Get the current stretched hash
            String currentHash = futureHashes.poll();
            futureHashes.add(generateStretchedHashBlake3(salt + (index + 1000)));

            // Check for triplets
            char tripletChar = getTripletChar(currentHash);
            if (tripletChar != '\0') {
                // Check next 1000 hashes for quintuplets
                for (String futureHash : futureHashes) {
                    if (hasQuintuplet(futureHash, tripletChar)) {
                        keysFound++;
                        if (keysFound == targetKeyIndex) {
                            return index;
                        }
                        break;
                    }
                }
            }

            index++;
        }

        return -1; // Shouldn't reach here
    }

    private static String generateStretchedHashBlake3(String input) {
        String hash = generateHashBlake3(input);
        for (int i = 0; i < 2016; i++) {
            hash = generateHashBlake3(hash);
        }
        return hash;
    }

    private static String generateHashBlake3(String input) {
        return Blake3.hex(input, 64);
    }

    private static char getTripletChar(String hash) {
        for (int i = 0; i < hash.length() - 2; i++) {
            if (hash.charAt(i) == hash.charAt(i + 1) && hash.charAt(i) == hash.charAt(i + 2)) {
                return hash.charAt(i);
            }
        }
        return '\0'; // No triplet found
    }

    private static boolean hasQuintuplet(String hash, char c) {
        String quintuplet = String.valueOf(c).repeat(5);
        return hash.contains(quintuplet);
    }
}
