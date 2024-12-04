package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	filePath := "input.txt"

	// Read the file and store the grid
	grid, err := readGrid(filePath)
	if err != nil {
		fmt.Println("Error reading file:", err)
		return
	}
	rows := len(grid)
	cols := len(grid[0])

	// Count occurrences of "XMAS"
	xmasCount := countXMAS(grid, rows, cols)
	fmt.Printf("Count of XMAS: %d\n", xmasCount)

	// Count all X-MAS patterns
	xmasPatterns := countAllXMASPatterns(grid, rows, cols)
	fmt.Printf("Total X-MAS patterns: %d\n", xmasPatterns)
}

// Reads the grid from the file
func readGrid(filePath string) ([]string, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var grid []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		grid = append(grid, scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return grid, nil
}

// Counts occurrences of the word "XMAS" in all directions
func countXMAS(grid []string, rows, cols int) int {
	targetWord := "XMAS"
	directions := [][2]int{
		{0, 1},   // Right
		{1, 0},   // Down
		{1, 1},   // Diagonal-right-down
		{1, -1},  // Diagonal-left-down
		{0, -1},  // Left
		{-1, 0},  // Up
		{-1, -1}, // Diagonal-left-up
		{-1, 1},  // Diagonal-right-up
	}

	count := 0

	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			for _, dir := range directions {
				if checkWord(grid, r, c, dir[0], dir[1], targetWord, rows, cols) {
					count++
				}
			}
		}
	}

	return count
}

// Checks if a word exists in the grid in the given direction
func checkWord(grid []string, x, y, dx, dy int, word string, rows, cols int) bool {
	for i := 0; i < len(word); i++ {
		nx := x + i*dx
		ny := y + i*dy
		if nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] != word[i] {
			return false
		}
	}
	return true
}

// Counts all X-MAS patterns
func countAllXMASPatterns(grid []string, rows, cols int) int {
	count := 0

	for r := 1; r < rows-1; r++ {
		for c := 1; c < cols-1; c++ {
			center := grid[r][c]
			topLeft := grid[r-1][c-1]
			topRight := grid[r-1][c+1]
			bottomLeft := grid[r+1][c-1]
			bottomRight := grid[r+1][c+1]

			if center == 'A' {
				// Pattern 1: M.S
				if topLeft == 'M' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'S' {
					count++
				}
				// Pattern 2: S.M
				if topLeft == 'S' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'M' {
					count++
				}
				// Pattern 3: M.M
				if topLeft == 'M' && topRight == 'M' && bottomLeft == 'S' && bottomRight == 'S' {
					count++
				}
				// Pattern 4: S.S
				if topLeft == 'S' && topRight == 'S' && bottomLeft == 'M' && bottomRight == 'M' {
					count++
				}
			}
		}
	}

	return count
}
