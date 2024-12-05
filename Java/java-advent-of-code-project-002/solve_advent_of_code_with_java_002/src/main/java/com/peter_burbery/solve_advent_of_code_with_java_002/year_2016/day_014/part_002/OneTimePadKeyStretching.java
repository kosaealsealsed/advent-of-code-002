package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_014.part_002;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedList;
import java.util.Queue;

public class OneTimePadKeyStretching {

    public static void main(String[] args) throws NoSuchAlgorithmException {
        String salt = "zpqevtbw"; // Replace with your puzzle input
        int targetKeyIndex = 64;

        int index = find64thKeyIndexWithStretching(salt, targetKeyIndex);
        System.out.println("Index producing the 64th key: " + index);
    }

    public static int find64thKeyIndexWithStretching(String salt, int targetKeyIndex) throws NoSuchAlgorithmException {
        MessageDigest md5 = MessageDigest.getInstance("MD5");

        Queue<String> futureHashes = new LinkedList<>();
        int keysFound = 0;
        int index = 0;

        // Precompute the first 1000 stretched hashes
        for (int i = 0; i < 1000; i++) {
            futureHashes.add(generateStretchedHash(md5, salt + i));
        }

        while (keysFound < targetKeyIndex) {
            // Get the current stretched hash
            String currentHash = futureHashes.poll();
            futureHashes.add(generateStretchedHash(md5, salt + (index + 1000)));

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

    private static String generateStretchedHash(MessageDigest md5, String input) {
        String hash = generateHash(md5, input);
        for (int i = 0; i < 2016; i++) {
            hash = generateHash(md5, hash);
        }
        return hash;
    }

    private static String generateHash(MessageDigest md5, String input) {
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

