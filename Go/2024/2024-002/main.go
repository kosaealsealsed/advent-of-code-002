package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Function to check if a report is safe
func isSafe(report []int) bool {
	differences := make([]int, len(report)-1)
	for i := 0; i < len(report)-1; i++ {
		differences[i] = report[i+1] - report[i]
	}

	allIncreasing := true
	allDecreasing := true

	for _, diff := range differences {
		if diff < 1 || diff > 3 {
			allIncreasing = false
		}
		if diff > -1 || diff < -3 {
			allDecreasing = false
		}
	}

	return allIncreasing || allDecreasing
}

// Function to check if a report is safe with the Problem Dampener
func isSafeWithDampener(report []int) bool {
	if isSafe(report) {
		return true
	}

	for i := 0; i < len(report); i++ {
		modifiedReport := append(report[:i], report[i+1:]...)
		if isSafe(modifiedReport) {
			return true
		}
	}

	return false
}

func main() {
	// Input file path
	inputPath := "input.txt"

	// Open the input file
	file, err := os.Open(inputPath)
	if err != nil {
		fmt.Println("Error opening file:", err)
		return
	}
	defer file.Close()

	// Read the file line by line
	var reports [][]int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Fields(line)
		report := make([]int, len(parts))
		for i, part := range parts {
			report[i], _ = strconv.Atoi(part)
		}
		reports = append(reports, report)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		return
	}

	// Count safe reports
	safeCount := 0
	for _, report := range reports {
		if isSafe(report) {
			safeCount++
		}
	}
	fmt.Printf("Safe reports: %d\n", safeCount)

	// Count safe reports with the Problem Dampener
	safeWithDampenerCount := 0
	for _, report := range reports {
		if isSafeWithDampener(report) {
			safeWithDampenerCount++
		}
	}
	fmt.Printf("Safe reports with dampener: %d\n", safeWithDampenerCount)
}
