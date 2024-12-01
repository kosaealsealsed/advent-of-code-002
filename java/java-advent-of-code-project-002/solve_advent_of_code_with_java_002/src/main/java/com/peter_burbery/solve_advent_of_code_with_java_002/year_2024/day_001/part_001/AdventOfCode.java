package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_001.part_001;

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
            
            // Sort both lists
            Collections.sort(leftList);
            Collections.sort(rightList);
            
            // Calculate the total distance
            int totalDistance = 0;
            for (int i = 0; i < leftList.size(); i++) {
                totalDistance += Math.abs(leftList.get(i) - rightList.get(i));
            }
            
            // Output the total distance
            System.out.println("Total Distance: " + totalDistance);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

