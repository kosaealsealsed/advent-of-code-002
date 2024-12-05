package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_017.part_001;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedList;
import java.util.Queue;

public class TwoStepsForward {

    private static final int VAULT_X = 3;
    private static final int VAULT_Y = 3;

    public static void main(String[] args) {
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_017\\input.txt";

        try {
            String passcode = readInput(filePath);

            // Start timing
            long startTime = System.nanoTime();

            // Find shortest path
            String shortestPath = findShortestPath(passcode);

            // End timing
            long endTime = System.nanoTime();
            double duration = (endTime - startTime) / 1_000_000_000.0;

            System.out.println("Shortest Path: " + shortestPath);
            System.out.printf("Execution Time: %.9f s%n", duration);

        } catch (IOException | NoSuchAlgorithmException e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    private static String readInput(String filePath) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
            return reader.readLine().trim();
        }
    }

    private static String findShortestPath(String passcode) throws NoSuchAlgorithmException {
        MessageDigest md5 = MessageDigest.getInstance("MD5");
        Queue<State> queue = new LinkedList<>();
        queue.add(new State(0, 0, ""));

        while (!queue.isEmpty()) {
            State current = queue.poll();

            // Check if we've reached the vault
            if (current.x == VAULT_X && current.y == VAULT_Y) {
                return current.path;
            }

            // Get the open doors for the current state
            String hash = md5Hash(md5, passcode + current.path);
            boolean[] openDoors = getOpenDoors(hash);

            // Enqueue valid moves
            if (openDoors[0] && current.y > 0) queue.add(new State(current.x, current.y - 1, current.path + "U"));
            if (openDoors[1] && current.y < 3) queue.add(new State(current.x, current.y + 1, current.path + "D"));
            if (openDoors[2] && current.x > 0) queue.add(new State(current.x - 1, current.y, current.path + "L"));
            if (openDoors[3] && current.x < 3) queue.add(new State(current.x + 1, current.y, current.path + "R"));
        }

        return ""; // Shouldn't reach here if a solution exists
    }

    private static String md5Hash(MessageDigest md5, String input) {
        byte[] hash = md5.digest(input.getBytes());
        StringBuilder hexString = new StringBuilder();
        for (byte b : hash) {
            hexString.append(String.format("%02x", b));
        }
        return hexString.toString();
    }

    private static boolean[] getOpenDoors(String hash) {
        boolean[] openDoors = new boolean[4];
        for (int i = 0; i < 4; i++) {
            char c = hash.charAt(i);
            openDoors[i] = (c >= 'b' && c <= 'f');
        }
        return openDoors;
    }

    private static class State {
        int x, y;
        String path;

        State(int x, int y, String path) {
            this.x = x;
            this.y = y;
            this.path = path;
        }
    }
}
