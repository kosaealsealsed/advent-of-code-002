// guardPatrolLoopDetector.js

const fs = require('fs');
const path = require('path');

// Direction mappings
const DIRECTION_MAP = {
    '^': 0,
    '>': 1,
    'v': 2,
    '<': 3
};

const DIRECTION_OFFSETS = [
    [-1, 0], // Up
    [0, 1],  // Right
    [1, 0],  // Down
    [0, -1]  // Left
];

/**
 * Helper class to represent a position in the grid.
 */
class Position {
    constructor(row, col) {
        this.row = row;
        this.col = col;
    }

    toString() {
        return `${this.row},${this.col}`;
    }
}

/**
 * Helper class to represent a state (position and direction) of the guard.
 */
class State {
    constructor(row, col, direction) {
        this.row = row;
        this.col = col;
        this.direction = direction;
    }

    toString() {
        return `${this.row},${this.col},${this.direction}`;
    }
}

/**
 * Simple Pair class to hold two related objects.
 */
class Pair {
    constructor(first, second) {
        this.first = first;
        this.second = second;
    }
}

/**
 * Parses the grid from the given file.
 *
 * @param {string} filePath Path to the input file.
 * @returns {Array<Array<string>>} 2D character array representing the grid.
 */
function parseGrid(filePath) {
    const gridList = [];
    const fileContent = fs.readFileSync(filePath, 'utf-8');
    const lines = fileContent.split(/\r?\n/);

    for (let line of lines) {
        line = line.trim();
        if (line.length > 0) {
            gridList.push(line.split(''));
        }
    }

    if (gridList.length === 0) {
        throw new Error("The grid is empty.");
    }

    const cols = gridList[0].length;
    for (let row of gridList) {
        if (row.length !== cols) {
            throw new Error("Inconsistent row lengths in the grid.");
        }
    }

    return gridList;
}

/**
 * Finds the guard's starting position and direction.
 *
 * @param {Array<Array<string>>} grid 2D character array representing the grid.
 * @returns {Pair} A Pair containing the starting Position and direction.
 */
function findGuard(grid) {
    for (let r = 0; r < grid.length; r++) {
        for (let c = 0; c < grid[0].length; c++) {
            const cell = grid[r][c];
            if (DIRECTION_MAP.hasOwnProperty(cell)) {
                const guardPos = new Position(r, c);
                const guardDir = DIRECTION_MAP[cell];
                grid[r][c] = '.'; // Clear the starting position
                return new Pair(guardPos, guardDir);
            }
        }
    }
    throw new Error("Guard not found in the grid.");
}

/**
 * Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
 *
 * @param {Array<Array<string>>} grid 2D character array representing the grid.
 * @param {Position} guardPos The starting position of the guard.
 * @returns {Array<Position>} List of possible obstruction Positions.
 */
function getPossibleObstructions(grid, guardPos) {
    const possible = [];
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
 * @param {Array<Array<string>>} grid 2D character array representing the grid.
 * @param {Position} startPos Starting position of the guard.
 * @param {number} startDir Starting direction of the guard.
 * @returns {boolean} True if a loop is detected, False if the guard exits the grid.
 */
function simulateMovement(grid, startPos, startDir) {
    const visitedStates = new Set();
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
 * Counts the number of distinct positions visited by the guard without any obstructions.
 *
 * @param {Array<Array<string>>} grid 2D character array representing the grid.
 * @param {Position} guardPos Starting position of the guard.
 * @param {number} guardDir Starting direction of the guard.
 * @returns {number} Number of distinct positions visited.
 */
function countDistinctPositionsVisited(grid, guardPos, guardDir) {
    const visitedPositions = new Set();
    visitedPositions.add(guardPos.toString());

    let r = guardPos.row;
    let c = guardPos.col;
    let direction = guardDir;

    while (true) {
        const [dr, dc] = DIRECTION_OFFSETS[direction];
        const newR = r + dr;
        const newC = c + dc;

        // Check boundaries
        if (newR < 0 || newR >= grid.length || newC < 0 || newC >= grid[0].length) {
            break; // Guard exits the mapped area
        }

        if (grid[newR][newC] === '#') {
            // Turn right if obstacle ahead
            direction = (direction + 1) % 4;
        } else {
            // Move forward
            r = newR;
            c = newC;
            visitedPositions.add(new Position(r, c).toString());
        }
    }

    return visitedPositions.size;
}

/**
 * Counts the number of obstruction positions that cause the guard to loop indefinitely.
 * Also measures and prints execution times.
 *
 * @param {Array<Array<string>>} grid 2D character array representing the grid.
 * @param {Position} guardPos Starting position of the guard.
 * @param {number} guardDir Starting direction of the guard.
 */
function countObstructionPositions(grid, guardPos, guardDir) {
    const totalStartTime = process.hrtime();

    // Clone the grid to prevent modifications
    const gridClone = grid.map(row => row.slice());

    // Time to find obstruction positions
    const obstructionStartTime = process.hrtime();
    const possibleObstructions = getPossibleObstructions(gridClone, guardPos);
    const obstructionEndTime = process.hrtime(obstructionStartTime);
    const obstructionTime = obstructionEndTime[0] + obstructionEndTime[1] / 1e9;

    // Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
    console.log("time, denominator");
    console.log(`${obstructionTime.toFixed(9)} ${possibleObstructions.length}`);

    // Print header for batches
    console.log("batch, batch time, cumulative time");

    let loopCount = 0;
    const total = possibleObstructions.length;
    const batchSize = 1000;
    let batchStartTime = process.hrtime();
    let cumulativeTime = obstructionTime;

    for (let idx = 0; idx < possibleObstructions.length; idx++) {
        const obstruction = possibleObstructions[idx];
        gridClone[obstruction.row][obstruction.col] = '#'; // Place obstruction

        if (simulateMovement(gridClone, guardPos, guardDir)) {
            loopCount++;
        }

        gridClone[obstruction.row][obstruction.col] = '.'; // Remove obstruction

        // Check if batch size is reached or it's the last position
        if ((idx + 1) % batchSize === 0 || (idx + 1) === total) {
            const batchEndTime = process.hrtime(batchStartTime);
            const batchTime = batchEndTime[0] + batchEndTime[1] / 1e9;
            cumulativeTime += batchTime;
            console.log(`${idx + 1} ${batchTime.toFixed(9)} ${cumulativeTime.toFixed(9)}`);
            batchStartTime = process.hrtime(); // Reset batch start time
        }
    }

    const totalEndTime = process.hrtime(totalStartTime);
    const totalTime = totalEndTime[0] + totalEndTime[1] / 1e9;

    // Print final answer header and line: [answer] [answer_time]
    console.log("answer, answer time");
    console.log(`${loopCount} ${totalTime.toFixed(9)}`);
}

/**
 * Main function to execute the Guard Patrol Loop Detector.
 */
function main() {
    // Specify the path to your input file here
    const filePath = path.join('\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-006\\input.txt');

    try {
        // Parse the grid
        const grid = parseGrid(filePath);

        // Find the guard's starting position and direction
        const guardInfo = findGuard(grid);
        const guardPos = guardInfo.first;
        const guardDir = guardInfo.second;

        // Part 1: Count distinct positions visited without obstructions
        const distinctPositions = countDistinctPositionsVisited(grid, guardPos, guardDir);
        console.log(`Number of distinct positions visited: ${distinctPositions}`);

        // Part 2: Detect loops with obstructions and measure execution times
        countObstructionPositions(grid, guardPos, guardDir);
    } catch (error) {
        console.error(`Error: ${error.message}`);
    }
}

// Execute the main function
main();
