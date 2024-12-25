package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"strconv"
	"strings"
)

// Function to calculate the total similarity score
func calculateSimilarityScore(filePath string) {
	// Read the file content
	data, err := ioutil.ReadFile(filePath)
	if err != nil {
		log.Fatalf("Failed to read file: %v", err)
	}

	// Split the file content into lines
	lines := strings.Split(string(data), "\n")

	var leftList []int
	var rightList []int

	// Read each line and extract the two numbers
	for _, line := range lines {
		// Skip empty lines
		if line == "" {
			continue
		}
		// Split the line into two parts
		parts := strings.Fields(line)
		if len(parts) != 2 {
			continue
		}

		// Convert the string to integers
		left, err := strconv.Atoi(parts[0])
		if err != nil {
			log.Fatalf("Failed to convert left value: %v", err)
		}

		right, err := strconv.Atoi(parts[1])
		if err != nil {
			log.Fatalf("Failed to convert right value: %v", err)
		}

		// Append to the lists
		leftList = append(leftList, left)
		rightList = append(rightList, right)
	}

	// Create a map to count occurrences of each number in the right list
	rightListCounts := make(map[int]int)
	for _, num := range rightList {
		rightListCounts[num]++
	}

	// Calculate the similarity score
	similarityScore := 0
	for _, left := range leftList {
		similarityScore += left * rightListCounts[left] // Multiply left by count in right list
	}

	// Output the similarity score
	fmt.Printf("Total Similarity Score: %d\n", similarityScore)
}

func main() {
	// File path for input data
	filePath := `\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-001\input.txt`

	// Call the function to calculate the similarity score
	calculateSimilarityScore(filePath)
}
