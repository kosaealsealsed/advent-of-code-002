const fs = require('fs');
const path = require('path');

const INPUT_FILE = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-010\\input.txt";

function readMap(filename) {
    const grid = [];
    const lines = fs.readFileSync(filename, 'utf-8').split('\n');
    for (const line of lines) {
        if (line.trim()) {
            grid.push(line.trim().split('').map(Number));
        }
    }
    return grid;
}

function neighbors(r, c, rows, cols) {
    const directions = [
        [-1, 0], [1, 0], [0, -1], [0, 1]
    ];
    const result = [];
    for (const [dr, dc] of directions) {
        const nr = r + dr;
        const nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            result.push([nr, nc]);
        }
    }
    return result;
}

function findTrailheadScores(grid) {
    const rows = grid.length;
    const cols = rows > 0 ? grid[0].length : 0;

    const trailheads = [];
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (grid[r][c] === 0) {
                trailheads.push([r, c]);
            }
        }
    }

    let totalScore = 0;

    for (const [startR, startC] of trailheads) {
        const visited = new Set();
        const queue = [[startR, startC]];
        visited.add(`${startR},${startC}`);

        const reachableNines = new Set();

        while (queue.length > 0) {
            const [r, c] = queue.shift();
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

function countPaths(r, c, grid, dp, rows, cols) {
    if (dp[r][c] !== -1) {
        return dp[r][c];
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

function calculateTotalRating(grid) {
    const rows = grid.length;
    const cols = rows > 0 ? grid[0].length : 0;

    const trailheads = [];
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (grid[r][c] === 0) {
                trailheads.push([r, c]);
            }
        }
    }

    const dp = Array.from({ length: rows }, () => Array(cols).fill(-1));

    let totalRating = 0;
    for (const [tr, tc] of trailheads) {
        totalRating += countPaths(tr, tc, grid, dp, rows, cols);
    }

    return totalRating;
}

function main() {
    const startTimePart1 = process.hrtime.bigint();
    const grid = readMap(INPUT_FILE);
    const totalScore = findTrailheadScores(grid);
    const endTimePart1 = process.hrtime.bigint();

    console.log(`Part 1 Result: ${totalScore}`);
    console.log(`Time taken for Part 1: ${(Number(endTimePart1 - startTimePart1) / 1e9).toFixed(9)} s`);

    const startTimePart2 = process.hrtime.bigint();
    const totalRating = calculateTotalRating(grid);
    const endTimePart2 = process.hrtime.bigint();

    console.log(`Part 2 Result: ${totalRating}`);
    console.log(`Time taken for Part 2: ${(Number(endTimePart2 - startTimePart2) / 1e9).toFixed(9)} s`);
}

main();
