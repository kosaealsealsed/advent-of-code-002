package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

const INPUT_FILE = `input.txt`

func readMap(filename string) [][]int {
	file, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var grid [][]int
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if len(line) > 0 {
			row := []int{}
			for _, ch := range line {
				num, _ := strconv.Atoi(string(ch))
				row = append(row, num)
			}
			grid = append(grid, row)
		}
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	return grid
}

func neighbors(r, c, rows, cols int) [][2]int {
	directions := [][2]int{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}
	var result [][2]int
	for _, dir := range directions {
		nr, nc := r+dir[0], c+dir[1]
		if nr >= 0 && nr < rows && nc >= 0 && nc < cols {
			result = append(result, [2]int{nr, nc})
		}
	}
	return result
}

func findTrailheadScores(grid [][]int) int {
	rows := len(grid)
	cols := len(grid[0])

	var trailheads [][2]int
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == 0 {
				trailheads = append(trailheads, [2]int{r, c})
			}
		}
	}

	totalScore := 0

	for _, start := range trailheads {
		startR, startC := start[0], start[1]

		visited := map[[2]int]bool{}
		queue := [][2]int{{startR, startC}}
		visited[[2]int{startR, startC}] = true

		reachableNines := map[[2]int]bool{}

		for len(queue) > 0 {
			r, c := queue[0][0], queue[0][1]
			queue = queue[1:]

			currentHeight := grid[r][c]

			if currentHeight == 9 {
				reachableNines[[2]int{r, c}] = true
			} else {
				nextHeight := currentHeight + 1
				for _, neighbor := range neighbors(r, c, rows, cols) {
					nr, nc := neighbor[0], neighbor[1]
					if !visited[[2]int{nr, nc}] && grid[nr][nc] == nextHeight {
						visited[[2]int{nr, nc}] = true
						queue = append(queue, [2]int{nr, nc})
					}
				}
			}
		}

		totalScore += len(reachableNines)
	}

	return totalScore
}

func countPaths(r, c int, grid [][]int, dp [][]int, rows, cols int) int {
	if dp[r][c] != -1 {
		return dp[r][c]
	}

	currentHeight := grid[r][c]

	if currentHeight == 9 {
		dp[r][c] = 1
		return 1
	}

	totalPaths := 0
	nextHeight := currentHeight + 1
	for _, neighbor := range neighbors(r, c, rows, cols) {
		nr, nc := neighbor[0], neighbor[1]
		if grid[nr][nc] == nextHeight {
			totalPaths += countPaths(nr, nc, grid, dp, rows, cols)
		}
	}

	dp[r][c] = totalPaths
	return totalPaths
}

func calculateTotalRating(grid [][]int) int {
	rows := len(grid)
	cols := len(grid[0])

	var trailheads [][2]int
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if grid[r][c] == 0 {
				trailheads = append(trailheads, [2]int{r, c})
			}
		}
	}

	dp := make([][]int, rows)
	for i := range dp {
		dp[i] = make([]int, cols)
		for j := range dp[i] {
			dp[i][j] = -1
		}
	}

	totalRating := 0
	for _, start := range trailheads {
		tr, tc := start[0], start[1]
		totalRating += countPaths(tr, tc, grid, dp, rows, cols)
	}

	return totalRating
}

func main() {
	startTimePart1 := time.Now()
	grid := readMap(INPUT_FILE)
	totalScore := findTrailheadScores(grid)
	endTimePart1 := time.Now()
	fmt.Printf("Part 1 Result: %d\n", totalScore)
	fmt.Printf("Time taken for Part 1: %.9f s\n", endTimePart1.Sub(startTimePart1).Seconds())

	startTimePart2 := time.Now()
	totalRating := calculateTotalRating(grid)
	endTimePart2 := time.Now()
	fmt.Printf("Part 2 Result: %d\n", totalRating)
	fmt.Printf("Time taken for Part 2: %.9f s\n", endTimePart2.Sub(startTimePart2).Seconds())
}
