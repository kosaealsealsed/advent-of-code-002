// GuardPatrolLoopDetector.ts

import * as fs from 'fs';
import * as path from 'path';

/**
 * Direction mappings and offsets.
 */
const DIRECTION_MAP: { [key: string]: number } = {
    '^': 0,
    '>': 1,
    'v': 2,
    '<': 3,
};

const DIRECTION_OFFSETS: number[][] = [
    [-1, 0], // Up
    [0, 1],  // Right
    [1, 0],  // Down
    [0, -1], // Left
];

/**
 * Position class to represent a cell in the grid.
 */
class Position {
    row: number;
    col: number;

    constructor(row: number, col: number) {
        this.row = row;
        this.col = col;
    }

    toString(): string {
        return `${this.row},${this.col}`;
    }

    static fromString(str: string): Position {
        const [row, col] = str.split(',').map(Number);
        return new Position(row, col);
    }
}

/**
 * State class to represent the guard's state (position and direction).
 */
class State {
    row: number;
    col: number;
    direction: number;

    constructor(row: number, col: number, direction: number) {
        this.row = row;
        this.col = col;
        this.direction = direction;
    }

    toString(): string {
        return `${this.row},${this.col},${this.direction}`;
    }

    static fromString(str: string): State {
        const [row, col, direction] = str.split(',').map(Number);
        return new State(row, col, direction);
    }
}

/**
 * Pair class to hold two related objects.
 */
class Pair<F, S> {
    first: F;
    second: S;

    constructor(first: F, second: S) {
        this.first = first;
        this.second = second;
    }
}

/**
 * Main class to detect guard patrol loops.
 */
class GuardPatrolLoopDetector {
    /**
     * Entry point of the program.
     * @param args Command-line arguments.
     */
    static main(args: string[]): void {
        const filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt"; // Specify the path to your input file here

        try {
            // Part 1: Count distinct positions visited without obstructions
            const distinctPositions = GuardPatrolLoopDetector.countDistinctPositionsVisited(filePath);
            console.log(`Number of distinct positions visited: ${distinctPositions}`);

            // Part 2: Detect loops with obstructions and measure execution times
            GuardPatrolLoopDetector.countObstructionPositions(filePath);
        } catch (error: any) {
            console.error(`Error: ${error.message}`);
        }
    }

    /**
     * Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
     *
     * @param filePath Path to the input file.
     * @return Number of distinct positions visited.
     */
    private static countDistinctPositionsVisited(filePath: string): number {
        // Parse the grid
        const grid = GuardPatrolLoopDetector.parseGrid(filePath);

        // Find the guard's starting position and direction
        const guardInfo = GuardPatrolLoopDetector.findGuard(grid);
        let guardPos = guardInfo.first;
        let guardDir = guardInfo.second;

        // Initialize visited positions set
        const visitedPositions: Set<string> = new Set();
        visitedPositions.add(guardPos.toString());

        // Simulate the guard's movement
        while (true) {
            const [dr, dc] = DIRECTION_OFFSETS[guardDir];
            const newR = guardPos.row + dr;
            const newC = guardPos.col + dc;

            // Check boundaries
            if (newR < 0 || newR >= grid.length || newC < 0 || newC >= grid[0].length) {
                break; // Guard exits the mapped area
            }

            if (grid[newR][newC] === '#') {
                // Turn right if obstacle ahead
                guardDir = (guardDir + 1) % 4;
            } else {
                // Move forward
                guardPos = new Position(newR, newC);
                visitedPositions.add(guardPos.toString());
            }
        }

        // Number of distinct positions visited
        return visitedPositions.size;
    }

    /**
     * Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
     * Also measures and prints execution times.
     *
     * @param filePath Path to the input file.
     */
    private static countObstructionPositions(filePath: string): void {
        // Start total timing
        const totalStartTime = process.hrtime();

        // Parse the grid
        const grid = GuardPatrolLoopDetector.parseGrid(filePath);

        // Find the guard's starting position and direction
        const guardInfo = GuardPatrolLoopDetector.findGuard(grid);
        const guardPos = guardInfo.first;
        const guardDir = guardInfo.second;

        // Time to find obstruction positions
        const obstructionStartTime = process.hrtime();
        const possibleObstructions = GuardPatrolLoopDetector.getPossibleObstructions(grid, guardPos);
        const obstructionEndTime = process.hrtime(obstructionStartTime);
        const obstructionTime = GuardPatrolLoopDetector.hrtimeToSeconds(obstructionEndTime);

        // Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
        console.log("time, denominator");
        console.log(`${obstructionTime.toFixed(9)} ${possibleObstructions.length}`);

        // Print header for batches
        console.log("batch, batch time, cumulative time");

        // Initialize loop counter
        let loopCount = 0;
        const total = possibleObstructions.length;

        // Initialize timing for batches
        const batchSize = 1000;
        let batchStartTime = process.hrtime();
        let cumulativeTime = obstructionTime; // cumulative_time includes obstruction_time

        for (let idx = 0; idx < possibleObstructions.length; idx++) {
            const obstruction = possibleObstructions[idx];
            grid[obstruction.row][obstruction.col] = '#'; // Place obstruction

            if (GuardPatrolLoopDetector.simulateMovement(grid, guardPos, guardDir)) {
                loopCount++; // Found a position that causes a loop
            }

            grid[obstruction.row][obstruction.col] = '.'; // Remove obstruction

            // Check if batch size is reached or it's the last position
            if ((idx + 1) % batchSize === 0 || (idx + 1) === total) {
                const batchEndTime = process.hrtime(batchStartTime);
                const batchTime = GuardPatrolLoopDetector.hrtimeToSeconds(batchEndTime);
                cumulativeTime += batchTime;
                console.log(`${idx + 1} ${batchTime.toFixed(9)} ${cumulativeTime.toFixed(9)}`);
                batchStartTime = process.hrtime(); // Reset batch start time
            }
        }

        // End total timing
        const totalEndTime = process.hrtime(totalStartTime);
        const totalTime = GuardPatrolLoopDetector.hrtimeToSeconds(totalEndTime); // Total time from start to end

        // Print final answer header and line: [answer] [answer_time]
        console.log("answer, answer time");
        console.log(`${loopCount} ${totalTime.toFixed(9)}`);
    }

    /**
     * Parses the grid from the given file.
     *
     * @param filePath Path to the input file.
     * @return 2D character array representing the grid.
     */
    private static parseGrid(filePath: string): string[][] {
        const gridList: string[][] = [];
        const absolutePath = path.resolve(filePath);

        try {
            const data = fs.readFileSync(absolutePath, 'utf-8');
            const lines = data.split(/\r?\n/);
            for (const line of lines) {
                if (line.trim().length > 0) {
                    gridList.push(line.trim().split(''));
                }
            }
        } catch (error: any) {
            throw new Error(`Failed to read the file: ${error.message}`);
        }

        if (gridList.length === 0) {
            throw new Error("The grid is empty.");
        }

        // Ensure all rows have the same length
        const cols = gridList[0].length;
        for (const row of gridList) {
            if (row.length !== cols) {
                throw new Error("Inconsistent row lengths in the grid.");
            }
        }

        return gridList;
    }

    /**
     * Finds the guard's starting position and direction.
     *
     * @param grid 2D character array representing the grid.
     * @return A Pair containing the starting Position and direction.
     */
    private static findGuard(grid: string[][]): Pair<Position, number> {
        for (let r = 0; r < grid.length; r++) {
            for (let c = 0; c < grid[0].length; c++) {
                const cell = grid[r][c];
                if (DIRECTION_MAP.hasOwnProperty(cell)) {
                    const guardPos = new Position(r, c);
                    const guardDir = DIRECTION_MAP[cell];
                    grid[r][c] = '.'; // Clear the starting position
                    return new Pair<Position, number>(guardPos, guardDir);
                }
            }
        }
        throw new Error("Guard not found in the grid.");
    }

    /**
     * Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
     *
     * @param grid     2D character array representing the grid.
     * @param guardPos The starting position of the guard.
     * @return List of possible obstruction Positions.
     */
    private static getPossibleObstructions(grid: string[][], guardPos: Position): Position[] {
        const possible: Position[] = [];
        for (let r = 0; r < grid.length; r++) {
            for (let c = 0; c < grid[0].length; c++) {
                if ((r !== guardPos.row || c !== guardPos.col) && grid[r][c] === '.') {
                    possible.push(new Position(r, c));
                }
            }
        }
        return possible;
    }

    /**
     * Simulates the guard's movement on the grid.
     *
     * @param grid     2D character array representing the grid.
     * @param startPos Starting position of the guard.
     * @param startDir Starting direction of the guard.
     * @return True if a loop is detected, False if the guard exits the grid.
     */
    private static simulateMovement(grid: string[][], startPos: Position, startDir: number): boolean {
        const visitedStates: Set<string> = new Set();
        let r = startPos.row;
        let c = startPos.col;
        let direction = startDir;

        while (true) {
            const currentState = new State(r, c, direction).toString();
            if (visitedStates.has(currentState)) {
                return true; // Loop detected
            }
            visitedStates.add(currentState);

            const [dr, dc] = DIRECTION_OFFSETS[direction];
            const newR = r + dr;
            const newC = c + dc;

            // Check boundaries
            if (newR < 0 || newR >= grid.length || newC < 0 || newC >= grid[0].length) {
                return false; // Guard exits the grid
            }

            if (grid[newR][newC] === '#') {
                // Turn right if obstacle ahead
                direction = (direction + 1) % 4;
            } else {
                // Move forward
                r = newR;
                c = newC;
            }
        }
    }

    /**
     * Converts process.hrtime tuple to seconds.
     *
     * @param hrtime Tuple returned by process.hrtime().
     * @return Time in seconds as a number.
     */
    private static hrtimeToSeconds(hrtime: [number, number]): number {
        const [seconds, nanoseconds] = hrtime;
        return seconds + nanoseconds / 1e9;
    }
}

// Execute the main function with command-line arguments
GuardPatrolLoopDetector.main(process.argv.slice(2));
