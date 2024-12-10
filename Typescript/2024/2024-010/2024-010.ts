import * as fs from 'fs';

const INPUT_FILE = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-010\\input.txt';

function readMap(filename: string): number[][] {
    const lines = fs.readFileSync(filename, 'utf-8').split('\n').filter(line => line.trim() !== '');
    return lines.map(line => line.trim().split('').map(ch => parseInt(ch, 10)));
}

function neighbors(r: number, c: number, rows: number, cols: number): [number, number][] {
    const directions: [number, number][] = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    return directions
        .map(([dr, dc]) => [r + dr, c + dc] as [number, number])
        .filter(([nr, nc]) => nr >= 0 && nr < rows && nc >= 0 && nc < cols);
}

function findTrailheadScores(grid: number[][]): number {
    const rows = grid.length;
    const cols = grid[0].length;

    const trailheads: [number, number][] = [];
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (grid[r][c] === 0) {
                trailheads.push([r, c]);
            }
        }
    }

    let totalScore = 0;

    for (const [startR, startC] of trailheads) {
        const visited = new Set<string>();
        const queue: [number, number][] = [];
        const reachableNines = new Set<string>();

        queue.push([startR, startC]);
        visited.add(`${startR},${startC}`);

        while (queue.length > 0) {
            const [r, c] = queue.shift()!;
            const currentHeight = grid[r][c];

            if (currentHeight === 9) {
                reachableNines.add(`${r},${c}`);
            } else {
                const nextHeight = currentHeight + 1;
                for (const [nr, nc] of neighbors(r, c, rows, cols)) {
                    if (!visited.has(`${nr},${nc}`) && grid[nr][nc] === nextHeight) {
                        visited.add(`${nr},${nc}`);
                        queue.push([nr, nc]);
                    }
                }
            }
        }

        totalScore += reachableNines.size;
    }

    return totalScore;
}

function countPaths(
    r: number,
    c: number,
    grid: number[][],
    dp: (number | null)[][],
    rows: number,
    cols: number
): number {
    if (dp[r][c] !== null) {
        return dp[r][c]!;
    }

    const currentHeight = grid[r][c];

    if (currentHeight === 9) {
        dp[r][c] = 1;
        return 1;
    }

    let totalPaths = 0;
    const nextHeight = currentHeight + 1;
    for (const [nr, nc] of neighbors(r, c, rows, cols)) {
        if (grid[nr][nc] === nextHeight) {
            totalPaths += countPaths(nr, nc, grid, dp, rows, cols);
        }
    }

    dp[r][c] = totalPaths;
    return totalPaths;
}

function calculateTotalRating(grid: number[][]): number {
    const rows = grid.length;
    const cols = grid[0].length;

    const trailheads: [number, number][] = [];
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (grid[r][c] === 0) {
                trailheads.push([r, c]);
            }
        }
    }

    const dp: (number | null)[][] = Array.from({ length: rows }, () => Array(cols).fill(null));

    return trailheads.reduce((total, [tr, tc]) => total + countPaths(tr, tc, grid, dp, rows, cols), 0);
}

function main() {
    const startTimePart1 = performance.now();
    const grid = readMap(INPUT_FILE);
    const totalScore = findTrailheadScores(grid);
    const endTimePart1 = performance.now();

    console.log(`Part 1 Result: ${totalScore}`);
    console.log(`Time taken for Part 1: ${(endTimePart1 - startTimePart1) / 1000}s`);

    const startTimePart2 = performance.now();
    const totalRating = calculateTotalRating(grid);
    const endTimePart2 = performance.now();

    console.log(`Part 2 Result: ${totalRating}`);
    console.log(`Time taken for Part 2: ${(endTimePart2 - startTimePart2) / 1000}s`);
}

main();
