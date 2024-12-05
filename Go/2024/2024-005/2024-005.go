package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"strconv"
)

// Split a string by a delimiter and return a slice of tokens
func split(str, delimiter string) []string {
	return strings.Split(str, delimiter)
}

// Trim whitespace from both ends of a string
func trim(s string) string {
	return strings.TrimSpace(s)
}

// Check if an update is ordered according to the rules
func isUpdateOrdered(update []int, rules [][2]int) bool {
	indexMap := make(map[int]int)
	for i, page := range update {
		indexMap[page] = i
	}

	for _, rule := range rules {
		x, y := rule[0], rule[1]
		if posX, okX := indexMap[x]; okX {
			if posY, okY := indexMap[y]; okY {
				if posX > posY {
					return false
				}
			}
		}
	}
	return true
}

// Perform topological sort on an update according to the rules
func topologicalSortUpdate(update []int, rules [][2]int) []int {
	graph := make(map[int][]int)
	inDegree := make(map[int]int)
	nodes := make(map[int]bool)

	// Initialize graph and in-degree
	for _, node := range update {
		graph[node] = []int{}
		inDegree[node] = 0
		nodes[node] = true
	}

	// Build the graph based on rules
	for _, rule := range rules {
		x, y := rule[0], rule[1]
		if nodes[x] && nodes[y] {
			graph[x] = append(graph[x], y)
			inDegree[y]++
		}
	}

	// Initialize queue with nodes having in-degree 0
	queue := []int{}
	for node := range nodes {
		if inDegree[node] == 0 {
			queue = append(queue, node)
		}
	}

	sortedUpdate := []int{}

	for len(queue) > 0 {
		// Dequeue
		current := queue[0]
		queue = queue[1:]
		sortedUpdate = append(sortedUpdate, current)

		// Decrease in-degree of neighbors
		for _, neighbor := range graph[current] {
			inDegree[neighbor]--
			if inDegree[neighbor] == 0 {
				queue = append(queue, neighbor)
			}
		}
	}

	// Check if topological sort was possible (i.e., no cycles)
	if len(sortedUpdate) != len(nodes) {
		// Cycle detected or unable to sort
		return []int{}
	}

	return sortedUpdate
}

// Get the middle page of an update
func getMiddlePage(update []int) int {
	return update[len(update)/2]
}

func main() {
	filePath := "input.txt"

	// Open the file
	file, err := os.Open(filePath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: Unable to open the file %s\n", filePath)
		return
	}
	defer file.Close()

	// Read the entire file content
	scanner := bufio.NewScanner(file)
	content := ""
	for scanner.Scan() {
		content += scanner.Text() + "\n"
	}
	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading the file: %v\n", err)
		return
	}
	content = strings.TrimSpace(content)

	// Split content into rules and updates based on two consecutive newlines
	sections := split(content, "\n\n")
	if len(sections) != 2 {
		fmt.Fprintln(os.Stderr, "Invalid input format. Expected two sections separated by two newlines.")
		return
	}

	rulesSection := sections[0]
	updatesSection := sections[1]

	// Parse rules
	var rules [][2]int
	ruleLines := split(rulesSection, "\n")
	for _, ruleLineRaw := range ruleLines {
		ruleLine := trim(ruleLineRaw)
		if ruleLine == "" {
			continue
		}
		parts := split(ruleLine, "|")
		if len(parts) != 2 {
			fmt.Fprintf(os.Stderr, "Invalid rule format: %s\n", ruleLine)
			continue
		}
		x, err1 := strconv.Atoi(trim(parts[0]))
		y, err2 := strconv.Atoi(trim(parts[1]))
		if err1 != nil || err2 != nil {
			fmt.Fprintf(os.Stderr, "Invalid number in rule: %s\n", ruleLine)
			continue
		}
		rules = append(rules, [2]int{x, y})
	}

	// Parse updates
	var updates [][]int
	updateLines := split(updatesSection, "\n")
	for _, updateLineRaw := range updateLines {
		updateLine := trim(updateLineRaw)
		if updateLine == "" {
			continue
		}
		parts := split(updateLine, ",")
		update := []int{}
		valid := true
		for _, partRaw := range parts {
			part := trim(partRaw)
			page, err := strconv.Atoi(part)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Invalid number in update: %s\n", updateLine)
				valid = false
				break
			}
			update = append(update, page)
		}
		if valid {
			updates = append(updates, update)
		}
	}

	// Identify correctly ordered updates and their middle page numbers
	correctUpdates := [][]int{}
	middlePages := []int{}

	for _, update := range updates {
		if isUpdateOrdered(update, rules) {
			correctUpdates = append(correctUpdates, update)
			middlePages = append(middlePages, getMiddlePage(update))
		}
	}

	// Calculate the sum of middle pages for correct updates
	sumMiddlePages := 0
	for _, page := range middlePages {
		sumMiddlePages += page
	}
	fmt.Printf("Sum of middle pages for correctly ordered updates: %d\n", sumMiddlePages)

	// Identify incorrectly ordered updates, correct them, and collect their middle pages
	incorrectUpdates := [][]int{}
	incorrectMiddlePages := []int{}

	for _, update := range updates {
		if !isUpdateOrdered(update, rules) {
			correctedUpdate := topologicalSortUpdate(update, rules)
			if len(correctedUpdate) == 0 {
				fmt.Fprintf(os.Stderr, "Cycle detected or unable to sort update: %v\n", update)
				continue
			}
			incorrectUpdates = append(incorrectUpdates, correctedUpdate)
			incorrectMiddlePages = append(incorrectMiddlePages, getMiddlePage(correctedUpdate))
		}
	}

	// Calculate the sum of middle pages for corrected updates
	sumIncorrectMiddlePages := 0
	for _, page := range incorrectMiddlePages {
		sumIncorrectMiddlePages += page
	}
	fmt.Printf("Sum of middle pages for corrected updates: %d\n", sumIncorrectMiddlePages)
}
