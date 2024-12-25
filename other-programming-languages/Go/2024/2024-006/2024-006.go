package main

import (
	"bufio"
	"fmt"
	"os"
	"time"
)

// Direction mappings
var DirectionMap = map[rune]int{
	'^': 0,
	'>': 1,
	'v': 2,
	'<': 3,
}

var DirectionOffsets = [4][2]int{
	{-1, 0}, // Up
	{0, 1},  // Right
	{1, 0},  // Down
	{0, -1}, // Left
}

// Position represents a position in the grid
type Position struct {
	Row int
	Col int
}

// State represents the state of the guard (position and direction)
type State struct {
	Row       int
	Col       int
	Direction int
}

func main() {
	// Specify the path to your input file here
	filePath := `input.txt`

	// Part 1: Count distinct positions visited without obstructions
	distinctPositions, err := countDistinctPositionsVisited(filePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		return
	}
	fmt.Printf("Number of distinct positions visited: %d\n", distinctPositions)

	// Part 2: Detect loops with obstructions and measure execution times
	err = countObstructionPositions(filePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		return
	}
}

// countDistinctPositionsVisited counts the number of distinct positions visited by the guard without any obstructions.
func countDistinctPositionsVisited(filePath string) (int, error) {
	// Parse the grid
	grid, err := parseGrid(filePath)
	if err != nil {
		return 0, err
	}

	// Find the guard's starting position and direction
	guardPos, guardDir, err := findGuard(grid)
	if err != nil {
		return 0, err
	}

	// Initialize visited positions set
	visitedPositions := make(map[Position]struct{})
	visitedPositions[guardPos] = struct{}{}

	// Simulate the guard's movement
	for {
		dr := DirectionOffsets[guardDir][0]
		dc := DirectionOffsets[guardDir][1]
		newR := guardPos.Row + dr
		newC := guardPos.Col + dc

		// Check boundaries
		if newR < 0 || newR >= len(grid) || newC < 0 || newC >= len(grid[0]) {
			break // Guard exits the mapped area
		}

		if grid[newR][newC] == '#' {
			// Turn right if obstacle ahead
			guardDir = (guardDir + 1) % 4
		} else {
			// Move forward
			guardPos = Position{Row: newR, Col: newC}
			visitedPositions[guardPos] = struct{}{}
		}
	}

	// Number of distinct positions visited
	return len(visitedPositions), nil
}

// countObstructionPositions counts the number of obstruction positions that cause the guard to loop indefinitely.
// It also measures and prints execution times.
func countObstructionPositions(filePath string) error {
	// Start total timing
	totalStartTime := time.Now()

	// Parse the grid
	grid, err := parseGrid(filePath)
	if err != nil {
		return err
	}

	// Find the guard's starting position and direction
	guardPos, guardDir, err := findGuard(grid)
	if err != nil {
		return err
	}

	// Time to find obstruction positions
	obstructionStartTime := time.Now()
	possibleObstructions := getPossibleObstructions(grid, guardPos)
	obstructionEndTime := time.Now()
	obstructionTime := obstructionEndTime.Sub(obstructionStartTime).Seconds()

	// Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
	fmt.Println("time, denominator")
	fmt.Printf("%.9f %d\n", obstructionTime, len(possibleObstructions))

	// Print header for batches
	fmt.Println("batch, batch time, cumulative time")

	// Initialize loop counter
	loopCount := 0
	total := len(possibleObstructions)

	// Initialize timing for batches
	const batchSize = 1000
	batchStartTime := time.Now()
	cumulativeTime := obstructionTime // cumulative_time includes obstruction_time

	for idx, obstruction := range possibleObstructions {
		// Place obstruction
		grid[obstruction.Row][obstruction.Col] = '#'

		// Simulate movement
		if simulateMovement(grid, guardPos, guardDir) {
			loopCount++
		}

		// Remove obstruction
		grid[obstruction.Row][obstruction.Col] = '.'

		// Check if batch size is reached or it's the last position
		if (idx+1)%batchSize == 0 || (idx+1) == total {
			batchEndTime := time.Now()
			batchTime := batchEndTime.Sub(batchStartTime).Seconds()
			cumulativeTime += batchTime
			fmt.Printf("%d %.9f %.9f\n", idx+1, batchTime, cumulativeTime)
			batchStartTime = time.Now() // Reset batch start time
		}
	}

	// End total timing
	totalEndTime := time.Now()
	totalTime := totalEndTime.Sub(totalStartTime).Seconds() // Total time from start to end

	// Print final answer header and line: [answer] [answer_time]
	fmt.Println("answer, answer time")
	fmt.Printf("%d %.9f\n", loopCount, totalTime)

	return nil
}

// parseGrid parses the grid from the given file.
func parseGrid(filePath string) ([][]rune, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("unable to open file: %w", err)
	}
	defer file.Close()

	var grid [][]rune
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if len(line) > 0 {
			grid = append(grid, []rune(line))
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("error reading file: %w", err)
	}

	if len(grid) == 0 {
		return nil, fmt.Errorf("the grid is empty")
	}

	// Ensure all rows have the same length
	cols := len(grid[0])
	for _, row := range grid {
		if len(row) != cols {
			return nil, fmt.Errorf("inconsistent row lengths in the grid")
		}
	}

	return grid, nil
}

// findGuard finds the guard's starting position and direction.
func findGuard(grid [][]rune) (Position, int, error) {
	for r, row := range grid {
		for c, cell := range row {
			if dir, exists := DirectionMap[cell]; exists {
				guardPos := Position{Row: r, Col: c}
				guardDir := dir
				grid[r][c] = '.' // Clear the starting position
				return guardPos, guardDir, nil
			}
		}
	}
	return Position{}, 0, fmt.Errorf("guard not found in the grid")
}

// getPossibleObstructions retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
func getPossibleObstructions(grid [][]rune, guardPos Position) []Position {
	var possible []Position
	for r, row := range grid {
		for c, cell := range row {
			if (r != guardPos.Row || c != guardPos.Col) && cell == '.' {
				possible = append(possible, Position{Row: r, Col: c})
			}
		}
	}
	return possible
}

// simulateMovement simulates the guard's movement on the grid.
// It returns true if a loop is detected, false if the guard exits the grid.
func simulateMovement(grid [][]rune, startPos Position, startDir int) bool {
	visitedStates := make(map[State]struct{})
	r, c := startPos.Row, startPos.Col
	direction := startDir

	for {
		currentState := State{Row: r, Col: c, Direction: direction}
		if _, exists := visitedStates[currentState]; exists {
			return true // Loop detected
		}
		visitedStates[currentState] = struct{}{}

		dr := DirectionOffsets[direction][0]
		dc := DirectionOffsets[direction][1]
		newR := r + dr
		newC := c + dc

		// Check boundaries
		if newR < 0 || newR >= len(grid) || newC < 0 || newC >= len(grid[0]) {
			return false // Guard exits the grid
		}

		if grid[newR][newC] == '#' {
			// Turn right if obstacle ahead
			direction = (direction + 1) % 4
		} else {
			// Move forward
			r = newR
			c = newC
		}
	}
}
