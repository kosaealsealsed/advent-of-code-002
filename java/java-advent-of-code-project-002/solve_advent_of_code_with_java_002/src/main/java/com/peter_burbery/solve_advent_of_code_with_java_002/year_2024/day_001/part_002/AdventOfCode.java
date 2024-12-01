package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_001.part_002;

import java.io.*;
import java.util.*;

public class AdventOfCode {
    public static void main(String[] args) {
        // Define the file path
        String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2024\\day_001\\input.txt";
        
        try {
            // Read the file and load the location IDs into two lists
            List<Integer> leftList = new ArrayList<>();
            List<Integer> rightList = new ArrayList<>();
            
            BufferedReader br = new BufferedReader(new FileReader(filePath));
            String line;
            while ((line = br.readLine()) != null) {
                String[] parts = line.split("\\s+");
                leftList.add(Integer.parseInt(parts[0]));
                rightList.add(Integer.parseInt(parts[1]));
            }
            br.close();
            
            // Create a map to count occurrences of each number in the right list
            Map<Integer, Integer> rightListCounts = new HashMap<>();
            for (int num : rightList) {
                rightListCounts.put(num, rightListCounts.getOrDefault(num, 0) + 1);
            }
            
            // Calculate the similarity score
            int similarityScore = 0;
            for (int left : leftList) {
                similarityScore += left * rightListCounts.getOrDefault(left, 0);
            }
            
            // Output the similarity score
            System.out.println("Total Similarity Score: " + similarityScore);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

