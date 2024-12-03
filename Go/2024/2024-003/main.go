package main

import (
	"bufio"
	"fmt"
	"os"
	"regexp"
	"strconv"
)

// Function to calculate the sum of all mul(X, Y) operations
func sumMulOperations(corruptedMemory string) int {
	// Define the regex pattern for mul(X,Y)
	pattern := regexp.MustCompile(`mul\((\d{1,3}),(\d{1,3})\)`)
	matches := pattern.FindAllStringSubmatch(corruptedMemory, -1)
	total := 0

	// Iterate over all matches and calculate the sum of products
	for _, match := range matches {
		num1, _ := strconv.Atoi(match[1])
		num2, _ := strconv.Atoi(match[2])
		total += num1 * num2
	}

	return total
}

// Function to calculate the sum of enabled mul(X, Y) operations
func sumEnabledMulOperations(corruptedMemory string) int {
	// Define the regex pattern for do(), don't(), and mul(X,Y)
	pattern := regexp.MustCompile(`(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))`)
	matches := pattern.FindAllStringSubmatch(corruptedMemory, -1)
	totalSum := 0
	mulEnabled := true // mul instructions are enabled at the start

	// Iterate over all matches
	for _, match := range matches {
		fullMatch := match[1]

		if fullMatch == "do()" {
			mulEnabled = true
		} else if fullMatch == "don't()" {
			mulEnabled = false
		} else if match[2] != "" && match[3] != "" && mulEnabled {
			// This is a mul(X, Y) instruction
			num1, _ := strconv.Atoi(match[2])
			num2, _ := strconv.Atoi(match[3])
			totalSum += num1 * num2
		}
	}

	return totalSum
}

func main() {
	// Read the corrupted memory from 'input.txt'
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println("Error: Unable to open file input.txt")
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	corruptedMemory := ""
	for scanner.Scan() {
		corruptedMemory += scanner.Text()
	}

	// Calculate the results
	totalSumAllMulOperations := sumMulOperations(corruptedMemory)
	totalSumEnabledMulOperations := sumEnabledMulOperations(corruptedMemory)

	// Output the results
	fmt.Printf("The sum of all mul operations is: %d\n", totalSumAllMulOperations)
	fmt.Printf("The sum of enabled mul operations is: %d\n", totalSumEnabledMulOperations)
}
