package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_011;

import java.io.*;
import java.nio.file.Files;
import java.util.*;
import java.util.regex.*;

public class Parser {

    public static void main(String[] args) throws IOException {
        // File path to input.txt
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_011\\input.txt";
        
        // Parse the input file
        State initialState = parseInput(filePath);
        
        // Print the parsed state for verification
        System.out.println(initialState);
    }

    public static State parseInput(String filePath) throws IOException {
        // Define maps for generators and microchips
        Map<String, Integer> generatorFloors = new HashMap<>();
        Map<String, Integer> microchipFloors = new HashMap<>();
        
        // Read the input file
        List<String> lines = Files.readAllLines(new File(filePath).toPath());

        // Regex patterns for matching generators and microchips
        Pattern generatorPattern = Pattern.compile("(\\w+) generator");
        Pattern microchipPattern = Pattern.compile("(\\w+)-compatible microchip");

        // Process each line to determine items on each floor
        for (int floor = 0; floor < lines.size(); floor++) {
            String line = lines.get(floor);
            
            // Match generators
            Matcher generatorMatcher = generatorPattern.matcher(line);
            while (generatorMatcher.find()) {
                String element = generatorMatcher.group(1); // Extract element name
                generatorFloors.put(element, floor + 1);    // Floors are 1-based
            }

            // Match microchips
            Matcher microchipMatcher = microchipPattern.matcher(line);
            while (microchipMatcher.find()) {
                String element = microchipMatcher.group(1); // Extract element name
                microchipFloors.put(element, floor + 1);    // Floors are 1-based
            }
        }

        // Return the initial state
        return new State(1, generatorFloors, microchipFloors);
    }
}

