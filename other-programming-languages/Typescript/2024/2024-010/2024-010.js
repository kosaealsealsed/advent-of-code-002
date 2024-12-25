"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
var INPUT_FILE = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-010\\input.txt';
function readMap(filename) {
    var lines = fs.readFileSync(filename, 'utf-8').split('\n').filter(function (line) { return line.trim() !== ''; });
    return lines.map(function (line) { return line.trim().split('').map(function (ch) { return parseInt(ch, 10); }); });
}
function neighbors(r, c, rows, cols) {
    var directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    return directions
        .map(function (_a) {
        var dr = _a[0], dc = _a[1];
        return [r + dr, c + dc];
    })
        .filter(function (_a) {
        var nr = _a[0], nc = _a[1];
        return nr >= 0 && nr < rows && nc >= 0 && nc < cols;
    });
}
function findTrailheadScores(grid) {
    var rows = grid.length;
    var cols = grid[0].length;
    var trailheads = [];
    for (var r = 0; r < rows; r++) {
        for (var c = 0; c < cols; c++) {
            if (grid[r][c] === 0) {
                trailheads.push([r, c]);
            }
        }
    }
    var totalScore = 0;
    for (var _i = 0, trailheads_1 = trailheads; _i < trailheads_1.length; _i++) {
        var _a = trailheads_1[_i], startR = _a[0], startC = _a[1];
        var visited = new Set();
        var queue = [];
        var reachableNines = new Set();
        queue.push([startR, startC]);
        visited.add("".concat(startR, ",").concat(startC));
        while (queue.length > 0) {
            var _b = queue.shift(), r = _b[0], c = _b[1];
            var currentHeight = grid[r][c];
            if (currentHeight === 9) {
                reachableNines.add("".concat(r, ",").concat(c));
            }
            else {
                var nextHeight = currentHeight + 1;
                for (var _c = 0, _d = neighbors(r, c, rows, cols); _c < _d.length; _c++) {
                    var _e = _d[_c], nr = _e[0], nc = _e[1];
                    if (!visited.has("".concat(nr, ",").concat(nc)) && grid[nr][nc] === nextHeight) {
                        visited.add("".concat(nr, ",").concat(nc));
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
    if (dp[r][c] !== null) {
        return dp[r][c];
    }
    var currentHeight = grid[r][c];
    if (currentHeight === 9) {
        dp[r][c] = 1;
        return 1;
    }
    var totalPaths = 0;
    var nextHeight = currentHeight + 1;
    for (var _i = 0, _a = neighbors(r, c, rows, cols); _i < _a.length; _i++) {
        var _b = _a[_i], nr = _b[0], nc = _b[1];
        if (grid[nr][nc] === nextHeight) {
            totalPaths += countPaths(nr, nc, grid, dp, rows, cols);
        }
    }
    dp[r][c] = totalPaths;
    return totalPaths;
}
function calculateTotalRating(grid) {
    var rows = grid.length;
    var cols = grid[0].length;
    var trailheads = [];
    for (var r = 0; r < rows; r++) {
        for (var c = 0; c < cols; c++) {
            if (grid[r][c] === 0) {
                trailheads.push([r, c]);
            }
        }
    }
    var dp = Array.from({ length: rows }, function () { return Array(cols).fill(null); });
    return trailheads.reduce(function (total, _a) {
        var tr = _a[0], tc = _a[1];
        return total + countPaths(tr, tc, grid, dp, rows, cols);
    }, 0);
}
function main() {
    var startTimePart1 = performance.now();
    var grid = readMap(INPUT_FILE);
    var totalScore = findTrailheadScores(grid);
    var endTimePart1 = performance.now();
    console.log("Part 1 Result: ".concat(totalScore));
    console.log("Time taken for Part 1: ".concat((endTimePart1 - startTimePart1) / 1000, "s"));
    var startTimePart2 = performance.now();
    var totalRating = calculateTotalRating(grid);
    var endTimePart2 = performance.now();
    console.log("Part 2 Result: ".concat(totalRating));
    console.log("Time taken for Part 2: ".concat((endTimePart2 - startTimePart2) / 1000, "s"));
}
main();
