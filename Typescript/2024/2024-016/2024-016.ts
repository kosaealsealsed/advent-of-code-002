import * as fs from 'fs';
import * as path from 'path';
import PriorityQueue = require('js-priority-queue');

/**
 * Reads and parses the input file
 * @param filename - The path to the input file
 * @returns List of maze lines
 */
function parseInput(filename: string): string[] {
    const filePath = path.resolve(__dirname, filename);
    const data = fs.readFileSync(filePath, 'utf8');
    return data.trim().split('\n').map(line => line.trim());
}

/**
 * Solve Part 1: Find the lowest possible score
 * @param mazeLines - List of maze lines
 * @returns The lowest possible score
 */
function solveMaze(mazeLines: string[]): number {
    const directions = [
        [0, 1],   // East
        [1, 0],   // South
        [0, -1],  // West
        [-1, 0]   // North
    ];

    const rows = mazeLines.length;
    const cols = mazeLines[0].length;
    let start: [number, number] | null = null;
    let end: [number, number] | null = null;

    // Locate start and end positions
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (mazeLines[r][c] === 'S') {
                start = [r, c];
            } else if (mazeLines[r][c] === 'E') {
                end = [r, c];
            }
        }
    }

    if (!start || !end) {
        throw new Error("Could not find 'S' or 'E' in the maze.");
    }

    const dist: number[][][] = Array.from({ length: rows }, () =>
        Array.from({ length: cols }, () => Array(4).fill(Infinity))
    );

    const pq = new PriorityQueue<[number, number, number, number]>({ comparator: (a, b) => a[0] - b[0] });
    dist[start[0]][start[1]][0] = 0;
    pq.queue([0, start[0], start[1], 0]); // cost, row, col, direction
    const visited = new Set<string>();

    while (pq.length > 0) {
        const [cost, r, c, d] = pq.dequeue();

        if (r === end[0] && c === end[1]) {
            return cost;
        }

        const stateKey = `${r},${c},${d}`;
        if (visited.has(stateKey)) continue;
        visited.add(stateKey);

        // Move forward
        const [dr, dc] = directions[d];
        const nr = r + dr;
        const nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] !== '#') {
            const newCost = cost + 1;
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost;
                pq.queue([newCost, nr, nc, d]);
            }
        }

        // Turn left
        const leftDir = (d + 3) % 4;
        const leftCost = cost + 1000;
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost;
            pq.queue([leftCost, r, c, leftDir]);
        }

        // Turn right
        const rightDir = (d + 1) % 4;
        const rightCost = cost + 1000;
        if (rightCost < dist[r][c][rightDir]) {
            dist[r][c][rightDir] = rightCost;
            pq.queue([rightCost, r, c, rightDir]);
        }
    }

    return -1; // No path found
}

/**
 * Solve Part 2: Count tiles on the best path
 * @param mazeLines - List of maze lines
 * @returns Number of tiles on the best path
 */
function solvePart2(mazeLines: string[]): number {
    const directions = [
        [0, 1],   // East
        [1, 0],   // South
        [0, -1],  // West
        [-1, 0]   // North
    ];

    const rows = mazeLines.length;
    const cols = mazeLines[0].length;
    let start: [number, number] | null = null;
    let end: [number, number] | null = null;

    // Locate start and end positions
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (mazeLines[r][c] === 'S') {
                start = [r, c];
            } else if (mazeLines[r][c] === 'E') {
                end = [r, c];
            }
        }
    }

    if (!start || !end) {
        throw new Error("Could not find 'S' or 'E' in the maze.");
    }

    const dist: number[][][] = Array.from({ length: rows }, () =>
        Array.from({ length: cols }, () => Array(4).fill(Infinity))
    );

    const pq = new PriorityQueue<[number, number, number, number]>({ comparator: (a, b) => a[0] - b[0] });
    dist[start[0]][start[1]][0] = 0;
    pq.queue([0, start[0], start[1], 0]); // cost, row, col, direction
    const visited = new Set<string>();

    while (pq.length > 0) {
        const [cost, r, c, d] = pq.dequeue();

        const stateKey = `${r},${c},${d}`;
        if (visited.has(stateKey)) continue;
        visited.add(stateKey);

        // Move forward
        const [dr, dc] = directions[d];
        const nr = r + dr;
        const nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] !== '#') {
            const newCost = cost + 1;
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost;
                pq.queue([newCost, nr, nc, d]);
            }
        }

        // Turn left
        const leftDir = (d + 3) % 4;
        const leftCost = cost + 1000;
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost;
            pq.queue([leftCost, r, c, leftDir]);
        }

        // Turn right
        const rightDir = (d + 1) % 4;
        const rightCost = cost + 1000;
        if (rightCost < dist[r][c][rightDir]) {
            dist[r][c][rightDir] = rightCost;
            pq.queue([rightCost, r, c, rightDir]);
        }
    }

    // Trace back the best path
    const minCostEnd = Math.min(...dist[end[0]][end[1]]);
    if (minCostEnd === Infinity) {
        return 0;
    }

    const onBestPath: boolean[][] = Array.from({ length: rows }, () => Array(cols).fill(false));
    const queue: [number, number, number][] = [];
    for (let d = 0; d < 4; d++) {
        if (dist[end[0]][end[1]][d] === minCostEnd) {
            queue.push([end[0], end[1], d]);
        }
    }

    const visitedReverse = new Set<string>();

    while (queue.length > 0) {
        const [r, c, d] = queue.shift()!;
        onBestPath[r][c] = true;

        const costHere = dist[r][c][d];

        // Reverse move forward
        const nr = r - directions[d][0];
        const nc = c - directions[d][1];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            if (mazeLines[nr][nc] !== '#' && dist[nr][nc][d] === costHere - 1) {
                const reverseKey = `${nr},${nc},${d}`;
                if (!visitedReverse.has(reverseKey)) {
                    visitedReverse.add(reverseKey);
                    queue.push([nr, nc, d]);
                }
            }
        }

        // Reverse turns
        for (const prevDir of [(d + 3) % 4, (d + 1) % 4]) {
            if (dist[r][c][prevDir] === costHere - 1000) {
                const reverseKey = `${r},${c},${prevDir}`;
                if (!visitedReverse.has(reverseKey)) {
                    visitedReverse.add(reverseKey);
                    queue.push([r, c, prevDir]);
                }
            }
        }
    }

    let tilesOnBestPath = 0;
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (onBestPath[r][c]) tilesOnBestPath++;
        }
    }

    return tilesOnBestPath;
}

/**
 * Main function
 */
function main(): void {
    try {
        const mazeLines = parseInput("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-016\\input.txt");
        const part1Result = solveMaze(mazeLines);
        console.log(`Lowest possible score (Part 1): ${part1Result}`);

        const part2Result = solvePart2(mazeLines);
        console.log(`Number of tiles on at least one best path (Part 2): ${part2Result}`);
    } catch (error) {
        console.error(error);
    }
}

main();
