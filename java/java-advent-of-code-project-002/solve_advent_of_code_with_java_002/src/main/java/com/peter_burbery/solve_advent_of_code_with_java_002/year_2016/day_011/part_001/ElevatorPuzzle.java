package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_011.part_001;

import java.io.IOException;
import java.util.*;
import com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_011.*;

public class ElevatorPuzzle {

	public static void main(String[] args) {
		try {
			// File path to the input file
			String filePath = "Z:\\C\\advent-of-code-002\\java\\java-advent-of-code-project-002\\solve_advent_of_code_with_java_002\\src\\main\\java\\com\\peter_burbery\\solve_advent_of_code_with_java_002\\year_2016\\day_011\\input.txt";

			// Parse the initial state from the input file
			State initialState = Parser.parseInput(filePath);

			// Start timing
			long startTime = System.nanoTime();

			// Perform BFS
			int steps = bfsWithLogging(initialState, 4);

			// End timing
			long endTime = System.nanoTime();

			// Print results
			System.out.println("Minimum steps: " + steps);
			System.out.println("Computation time: " + (endTime - startTime) + " nanoseconds");
		} catch (IOException e) {
			System.err.println("Error reading input file: " + e.getMessage());
		}
	}

	public static int bfsWithLogging(State initialState, int targetFloor) {
    Queue<State> queue = new LinkedList<>();
    Set<String> visited = new HashSet<>(); // Use canonical keys for visited states

    // Initialize the BFS queue and visited set
    queue.add(initialState);
    visited.add(initialState.getCanonicalKey());
    int steps = 0;
    int iteration = 0;

    // Perform BFS
    while (!queue.isEmpty()) {
        int size = queue.size();

        for (int i = 0; i < size; i++) {
            State current = queue.poll();

            // Check if the goal is reached
            if (current.isGoal(targetFloor)) {
                System.out.println("Solution found after " + iteration + " iterations and " + steps + " steps!");
                System.out.flush(); // Ensure the output is immediately printed
                return steps;
            }

            // Generate valid next states
            List<State> nextStates = generateNextStates(current);
            for (State nextState : nextStates) {
                String canonicalKey = nextState.getCanonicalKey();
                if (!visited.contains(canonicalKey) && nextState.isValid()) {
                    queue.add(nextState);
                    visited.add(canonicalKey);
                }
            }
        }

        steps++;
        iteration++;

        // Log progress every 2 iterations
        if (iteration % 5 == 0) {
            System.out.println("Iteration " + iteration + ":");
            System.out.println("  Current BFS Depth: " + steps);
            System.out.println("  Queue Size: " + queue.size());
            System.out.println("  States Explored: " + visited.size());
            System.out.flush(); // Ensure logs are printed immediately
        }
    }

    System.out.println("No solution found after " + iteration + " iterations.");
    System.out.flush(); // Ensure logs are printed immediately
    return -1; // No solution found
}


	public static List<State> generateNextStates(State current) {
		List<State> nextStates = new ArrayList<>();
		int currentFloor = current.elevatorFloor;

		// Identify all items on the current floor
		List<String> itemsOnCurrentFloor = new ArrayList<>();
		for (Map.Entry<String, Integer> entry : current.rtgFloors.entrySet()) {
			if (entry.getValue() == currentFloor)
				itemsOnCurrentFloor.add(entry.getKey() + "G");
		}
		for (Map.Entry<String, Integer> entry : current.chipFloors.entrySet()) {
			if (entry.getValue() == currentFloor)
				itemsOnCurrentFloor.add(entry.getKey() + "M");
		}

		// Generate all combinations of one or two items
		List<List<String>> combinations = generateCombinations(itemsOnCurrentFloor);

		// Try moving up and down for each combination
		for (List<String> combination : combinations) {
			// Move up
			if (currentFloor < 4) {
				State nextState = moveItems(current, combination, currentFloor + 1);
				if (nextState != null && nextState.isValid()) {
					nextStates.add(nextState);
				}
			}
			// Move down
			if (currentFloor > 1) {
				State nextState = moveItems(current, combination, currentFloor - 1);
				if (nextState != null && nextState.isValid()) {
					nextStates.add(nextState);
				}
			}
		}

		return nextStates;
	}

	// Generate all combinations of one or two items
	private static List<List<String>> generateCombinations(List<String> items) {
		List<List<String>> combinations = new ArrayList<>();
		// Single items
		for (String item : items) {
			combinations.add(List.of(item));
		}
		// Pairs of items
		for (int i = 0; i < items.size(); i++) {
			for (int j = i + 1; j < items.size(); j++) {
				combinations.add(List.of(items.get(i), items.get(j)));
			}
		}
		return combinations;
	}

	// Move items between floors
	private static State moveItems(State current, List<String> items, int targetFloor) {
		// Copy the current state
		Map<String, Integer> newRtgFloors = new HashMap<>(current.rtgFloors);
		Map<String, Integer> newChipFloors = new HashMap<>(current.chipFloors);

		// Move items to the target floor
		for (String item : items) {
			if (item.endsWith("G")) { // Generator
				String element = item.substring(0, item.length() - 1);
				newRtgFloors.put(element, targetFloor);
			} else if (item.endsWith("M")) { // Microchip
				String element = item.substring(0, item.length() - 1);
				newChipFloors.put(element, targetFloor);
			}
		}

		// Return the new state
		return new State(targetFloor, newRtgFloors, newChipFloors);
	}

}
