package main

import (
	"fmt"
	"io/ioutil"
	"log"
	"sort"
	"strconv"
	"strings"
)

// Function to calculate the total distance between two sorted lists
func calculateTotalDistance(filePath string) {
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

	// Sort both lists
	sort.Ints(leftList)
	sort.Ints(rightList)

	// Calculate the total distance
	totalDistance := 0
	for i := 0; i < len(leftList); i++ {
		totalDistance += abs(leftList[i] - rightList[i])
	}

	// Output the total distance
	fmt.Printf("Total Distance: %d\n", totalDistance)
}

// Helper function to calculate the absolute value
func abs(x int) int {
	if x < 0 {
		return -x
	}
	return x
}

func main() {
	// Define the file path for input data
	filePath := `\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-001\input.txt`

	// Call the function to calculate the total distance
	calculateTotalDistance(filePath)
}
