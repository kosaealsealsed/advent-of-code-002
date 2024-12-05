package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_014.part_002;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedList;
import java.util.Queue;

public class OneTimePadKeyStretchingMD5 {

    public static void main(String[] args) throws NoSuchAlgorithmException {
        String salt = "zpqevtbw"; // Replace with your puzzle input
        int targetKeyIndex = 64;

        // Timing for MD5
        long startTimeMD5 = System.nanoTime();
        int indexMD5 = find64thKeyIndexWithMD5Stretching(salt, targetKeyIndex);
        long endTimeMD5 = System.nanoTime();
        double durationMD5 = (endTimeMD5 - startTimeMD5) / 1_000_000_000.0;
        System.out.printf("MD5: Index producing the 64th key: %d in %.9f s%n", indexMD5, durationMD5);
    }

    public static int find64thKeyIndexWithMD5Stretching(String salt, int targetKeyIndex) throws NoSuchAlgorithmException {
        MessageDigest md5 = MessageDigest.getInstance("MD5");

        Queue<String> futureHashes = new LinkedList<>();
        int keysFound = 0;
        int index = 0;

        // Precompute the first 1000 stretched hashes
        for (int i = 0; i < 1000; i++) {
            futureHashes.add(generateStretchedHashMD5(md5, salt + i));
        }

        while (keysFound < targetKeyIndex) {
            // Get the current stretched hash
            String currentHash = futureHashes.poll();
            futureHashes.add(generateStretchedHashMD5(md5, salt + (index + 1000)));

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

    private static String generateStretchedHashMD5(MessageDigest md5, String input) {
        String hash = generateHashMD5(md5, input);
        for (int i = 0; i < 2016; i++) {
            hash = generateHashMD5(md5, hash);
        }
        return hash;
    }

    private static String generateHashMD5(MessageDigest md5, String input) {
        byte[] hash = md5.digest(input.getBytes());
        StringBuilder hexString = new StringBuilder();
        for (byte b : hash) {
            hexString.append(String.format("%02x", b));
        }
        return hexString.toString();
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
