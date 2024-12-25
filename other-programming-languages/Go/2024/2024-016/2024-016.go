package main

import (
	"container/heap"
	"container/list"
	"fmt"
	"io/ioutil"
	"log"
	"strings"
)

type State struct {
	Cost      int
	Row, Col  int
	Direction int
}

type PriorityQueue []State

func (pq PriorityQueue) Len() int           { return len(pq) }
func (pq PriorityQueue) Less(i, j int) bool { return pq[i].Cost < pq[j].Cost }
func (pq PriorityQueue) Swap(i, j int)      { pq[i], pq[j] = pq[j], pq[i] }

func (pq *PriorityQueue) Push(x interface{}) {
	*pq = append(*pq, x.(State))
}

func (pq *PriorityQueue) Pop() interface{} {
	old := *pq
	n := len(old)
	item := old[n-1]
	*pq = old[:n-1]
	return item
}

func solveMaze(mazeLines []string) (int, error) {
	// Directions: 0=East, 1=South, 2=West, 3=North
	directions := [][2]int{
		{0, 1},   // East
		{1, 0},   // South
		{0, -1},  // West
		{-1, 0},  // North
	}

	rows := len(mazeLines)
	cols := len(mazeLines[0])

	var start, end [2]int
	startFound, endFound := false, false

	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			switch mazeLines[r][c] {
			case 'S':
				start = [2]int{r, c}
				startFound = true
			case 'E':
				end = [2]int{r, c}
				endFound = true
			}
		}
	}

	if !startFound || !endFound {
		return -1, fmt.Errorf("could not find 'S' or 'E' in the maze")
	}

	const INF = int(1e9)
	dist := make([][][]int, rows)
	for i := range dist {
		dist[i] = make([][]int, cols)
		for j := range dist[i] {
			dist[i][j] = []int{INF, INF, INF, INF}
		}
	}

	startDir := 0 // Facing East
	dist[start[0]][start[1]][startDir] = 0

	pq := &PriorityQueue{}
	heap.Push(pq, State{Cost: 0, Row: start[0], Col: start[1], Direction: startDir})

	visited := make(map[[3]int]bool)

	for pq.Len() > 0 {
		state := heap.Pop(pq).(State)
		cost, r, c, d := state.Cost, state.Row, state.Col, state.Direction

		if [2]int{r, c} == end {
			return cost, nil
		}

		if visited[[3]int{r, c, d}] {
			continue
		}
		visited[[3]int{r, c, d}] = true

		currentDist := dist[r][c][d]
		if cost > currentDist {
			continue
		}

		// Move forward (cost + 1) if not a wall
		dr, dc := directions[d][0], directions[d][1]
		nr, nc := r+dr, c+dc
		if nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] != '#' {
			newCost := cost + 1
			if newCost < dist[nr][nc][d] {
				dist[nr][nc][d] = newCost
				heap.Push(pq, State{Cost: newCost, Row: nr, Col: nc, Direction: d})
			}
		}

		// Turn left (cost + 1000)
		leftDir := (d - 1 + 4) % 4
		newCost := cost + 1000
		if newCost < dist[r][c][leftDir] {
			dist[r][c][leftDir] = newCost
			heap.Push(pq, State{Cost: newCost, Row: r, Col: c, Direction: leftDir})
		}

		// Turn right (cost + 1000)
		rightDir := (d + 1) % 4
		newCost = cost + 1000
		if newCost < dist[r][c][rightDir] {
			dist[r][c][rightDir] = newCost
			heap.Push(pq, State{Cost: newCost, Row: r, Col: c, Direction: rightDir})
		}
	}

	return -1, nil
}

func solvePart2(mazeLines []string) (int, error) {
	directions := [][2]int{
		{0, 1}, {1, 0}, {0, -1}, {-1, 0}, // East, South, West, North
	}

	rows := len(mazeLines)
	cols := len(mazeLines[0])

	var start, end [2]int
	startFound, endFound := false, false

	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			switch mazeLines[r][c] {
			case 'S':
				start = [2]int{r, c}
				startFound = true
			case 'E':
				end = [2]int{r, c}
				endFound = true
			}
		}
	}

	if !startFound || !endFound {
		return -1, fmt.Errorf("could not find 'S' or 'E' in the maze")
	}

	const INF = int(1e9)
	dist := make([][][]int, rows)
	for i := range dist {
		dist[i] = make([][]int, cols)
		for j := range dist[i] {
			dist[i][j] = []int{INF, INF, INF, INF}
		}
	}

	startDir := 0
	dist[start[0]][start[1]][startDir] = 0

	pq := &PriorityQueue{}
	heap.Push(pq, State{Cost: 0, Row: start[0], Col: start[1], Direction: startDir})

	visited := make(map[[3]int]bool)

	for pq.Len() > 0 {
		state := heap.Pop(pq).(State)
		cost, r, c, d := state.Cost, state.Row, state.Col, state.Direction

		if visited[[3]int{r, c, d}] {
			continue
		}
		visited[[3]int{r, c, d}] = true

		currentDist := dist[r][c][d]
		if cost > currentDist {
			continue
		}

		dr, dc := directions[d][0], directions[d][1]
		nr, nc := r+dr, c+dc
		if nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] != '#' {
			newCost := cost + 1
			if newCost < dist[nr][nc][d] {
				dist[nr][nc][d] = newCost
				heap.Push(pq, State{Cost: newCost, Row: nr, Col: nc, Direction: d})
			}
		}

		leftDir := (d - 1 + 4) % 4
		newCost := cost + 1000
		if newCost < dist[r][c][leftDir] {
			dist[r][c][leftDir] = newCost
			heap.Push(pq, State{Cost: newCost, Row: r, Col: c, Direction: leftDir})
		}

		rightDir := (d + 1) % 4
		newCost = cost + 1000
		if newCost < dist[r][c][rightDir] {
			dist[r][c][rightDir] = newCost
			heap.Push(pq, State{Cost: newCost, Row: r, Col: c, Direction: rightDir})
		}
	}

	minCost := INF
	for d := 0; d < 4; d++ {
		if dist[end[0]][end[1]][d] < minCost {
			minCost = dist[end[0]][end[1]][d]
		}
	}

	if minCost == INF {
		return 0, nil
	}

	onBestPath := make([][]bool, rows)
	for i := range onBestPath {
		onBestPath[i] = make([]bool, cols)
	}

	type ReverseState struct {
		Row, Col, Direction int
	}

	revQueue := list.New()
	for d := 0; d < 4; d++ {
		if dist[end[0]][end[1]][d] == minCost {
			revQueue.PushBack(ReverseState{Row: end[0], Col: end[1], Direction: d})
		}
	}

	revVisited := make(map[ReverseState]bool)
	for revQueue.Len() > 0 {
		curr := revQueue.Remove(revQueue.Front()).(ReverseState)
		r, c, d := curr.Row, curr.Col, curr.Direction
		onBestPath[r][c] = true

		costHere := dist[r][c][d]

		dr, dc := directions[d][0], directions[d][1]
		rPrev, cPrev := r-dr, c-dc
		if rPrev >= 0 && rPrev < rows && cPrev >= 0 && cPrev < cols {
			if mazeLines[rPrev][cPrev] != '#' && dist[rPrev][cPrev][d] == costHere-1 {
				prevState := ReverseState{Row: rPrev, Col: cPrev, Direction: d}
				if !revVisited[prevState] {
					revVisited[prevState] = true
					revQueue.PushBack(prevState)
				}
			}
		}

		for _, dPre := range []int{(d - 1 + 4) % 4, (d + 1) % 4} {
			if dist[r][c][dPre] == costHere-1000 {
				prevState := ReverseState{Row: r, Col: c, Direction: dPre}
				if !revVisited[prevState] {
					revVisited[prevState] = true
					revQueue.PushBack(prevState)
				}
			}
		}
	}

	count := 0
	for r := 0; r < rows; r++ {
		for c := 0; c < cols; c++ {
			if onBestPath[r][c] {
				count++
			}
		}
	}

	return count, nil
}

func main() {
	data, err := ioutil.ReadFile("input.txt")
	if err != nil {
		log.Fatalf("failed to read file: %v", err)
	}

	lines := strings.Split(strings.TrimSpace(string(data)), "\n")
	result, err := solveMaze(lines)
	if err != nil {
		log.Fatalf("failed to solve maze: %v", err)
	}

	fmt.Printf("Lowest possible score: %d\n", result)

	result2, err := solvePart2(lines)
	if err != nil {
		log.Fatalf("failed to solve part 2: %v", err)
	}

	fmt.Printf("Number of tiles on at least one best path: %d\n", result2)
}
