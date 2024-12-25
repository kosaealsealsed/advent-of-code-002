"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
var path = require("path");
var PriorityQueue = require("js-priority-queue");
/**
 * Reads and parses the input file
 * @param filename - The path to the input file
 * @returns List of maze lines
 */
function parseInput(filename) {
    var filePath = path.resolve(__dirname, filename);
    var data = fs.readFileSync(filePath, 'utf8');
    return data.trim().split('\n').map(function (line) { return line.trim(); });
}
/**
 * Solve Part 1: Find the lowest possible score
 * @param mazeLines - List of maze lines
 * @returns The lowest possible score
 */
function solveMaze(mazeLines) {
    var directions = [
        [0, 1], // East
        [1, 0], // South
        [0, -1], // West
        [-1, 0] // North
    ];
    var rows = mazeLines.length;
    var cols = mazeLines[0].length;
    var start = null;
    var end = null;
    // Locate start and end positions
    for (var r = 0; r < rows; r++) {
        for (var c = 0; c < cols; c++) {
            if (mazeLines[r][c] === 'S') {
                start = [r, c];
            }
            else if (mazeLines[r][c] === 'E') {
                end = [r, c];
            }
        }
    }
    if (!start || !end) {
        throw new Error("Could not find 'S' or 'E' in the maze.");
    }
    var dist = Array.from({ length: rows }, function () {
        return Array.from({ length: cols }, function () { return Array(4).fill(Infinity); });
    });
    var pq = new PriorityQueue({ comparator: function (a, b) { return a[0] - b[0]; } });
    dist[start[0]][start[1]][0] = 0;
    pq.queue([0, start[0], start[1], 0]); // cost, row, col, direction
    var visited = new Set();
    while (pq.length > 0) {
        var _a = pq.dequeue(), cost = _a[0], r = _a[1], c = _a[2], d = _a[3];
        if (r === end[0] && c === end[1]) {
            return cost;
        }
        var stateKey = "".concat(r, ",").concat(c, ",").concat(d);
        if (visited.has(stateKey))
            continue;
        visited.add(stateKey);
        // Move forward
        var _b = directions[d], dr = _b[0], dc = _b[1];
        var nr = r + dr;
        var nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] !== '#') {
            var newCost = cost + 1;
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost;
                pq.queue([newCost, nr, nc, d]);
            }
        }
        // Turn left
        var leftDir = (d + 3) % 4;
        var leftCost = cost + 1000;
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost;
            pq.queue([leftCost, r, c, leftDir]);
        }
        // Turn right
        var rightDir = (d + 1) % 4;
        var rightCost = cost + 1000;
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
function solvePart2(mazeLines) {
    var directions = [
        [0, 1], // East
        [1, 0], // South
        [0, -1], // West
        [-1, 0] // North
    ];
    var rows = mazeLines.length;
    var cols = mazeLines[0].length;
    var start = null;
    var end = null;
    // Locate start and end positions
    for (var r = 0; r < rows; r++) {
        for (var c = 0; c < cols; c++) {
            if (mazeLines[r][c] === 'S') {
                start = [r, c];
            }
            else if (mazeLines[r][c] === 'E') {
                end = [r, c];
            }
        }
    }
    if (!start || !end) {
        throw new Error("Could not find 'S' or 'E' in the maze.");
    }
    var dist = Array.from({ length: rows }, function () {
        return Array.from({ length: cols }, function () { return Array(4).fill(Infinity); });
    });
    var pq = new PriorityQueue({ comparator: function (a, b) { return a[0] - b[0]; } });
    dist[start[0]][start[1]][0] = 0;
    pq.queue([0, start[0], start[1], 0]); // cost, row, col, direction
    var visited = new Set();
    while (pq.length > 0) {
        var _a = pq.dequeue(), cost = _a[0], r = _a[1], c = _a[2], d = _a[3];
        var stateKey = "".concat(r, ",").concat(c, ",").concat(d);
        if (visited.has(stateKey))
            continue;
        visited.add(stateKey);
        // Move forward
        var _b = directions[d], dr = _b[0], dc = _b[1];
        var nr = r + dr;
        var nc = c + dc;
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] !== '#') {
            var newCost = cost + 1;
            if (newCost < dist[nr][nc][d]) {
                dist[nr][nc][d] = newCost;
                pq.queue([newCost, nr, nc, d]);
            }
        }
        // Turn left
        var leftDir = (d + 3) % 4;
        var leftCost = cost + 1000;
        if (leftCost < dist[r][c][leftDir]) {
            dist[r][c][leftDir] = leftCost;
            pq.queue([leftCost, r, c, leftDir]);
        }
        // Turn right
        var rightDir = (d + 1) % 4;
        var rightCost = cost + 1000;
        if (rightCost < dist[r][c][rightDir]) {
            dist[r][c][rightDir] = rightCost;
            pq.queue([rightCost, r, c, rightDir]);
        }
    }
    // Trace back the best path
    var minCostEnd = Math.min.apply(Math, dist[end[0]][end[1]]);
    if (minCostEnd === Infinity) {
        return 0;
    }
    var onBestPath = Array.from({ length: rows }, function () { return Array(cols).fill(false); });
    var queue = [];
    for (var d = 0; d < 4; d++) {
        if (dist[end[0]][end[1]][d] === minCostEnd) {
            queue.push([end[0], end[1], d]);
        }
    }
    var visitedReverse = new Set();
    while (queue.length > 0) {
        var _c = queue.shift(), r = _c[0], c = _c[1], d = _c[2];
        onBestPath[r][c] = true;
        var costHere = dist[r][c][d];
        // Reverse move forward
        var nr = r - directions[d][0];
        var nc = c - directions[d][1];
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            if (mazeLines[nr][nc] !== '#' && dist[nr][nc][d] === costHere - 1) {
                var reverseKey = "".concat(nr, ",").concat(nc, ",").concat(d);
                if (!visitedReverse.has(reverseKey)) {
                    visitedReverse.add(reverseKey);
                    queue.push([nr, nc, d]);
                }
            }
        }
        // Reverse turns
        for (var _i = 0, _d = [(d + 3) % 4, (d + 1) % 4]; _i < _d.length; _i++) {
            var prevDir = _d[_i];
            if (dist[r][c][prevDir] === costHere - 1000) {
                var reverseKey = "".concat(r, ",").concat(c, ",").concat(prevDir);
                if (!visitedReverse.has(reverseKey)) {
                    visitedReverse.add(reverseKey);
                    queue.push([r, c, prevDir]);
                }
            }
        }
    }
    var tilesOnBestPath = 0;
    for (var r = 0; r < rows; r++) {
        for (var c = 0; c < cols; c++) {
            if (onBestPath[r][c])
                tilesOnBestPath++;
        }
    }
    return tilesOnBestPath;
}
/**
 * Main function
 */
function main() {
    try {
        var mazeLines = parseInput("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-016\\input.txt");
        var part1Result = solveMaze(mazeLines);
        console.log("Lowest possible score (Part 1): ".concat(part1Result));
        var part2Result = solvePart2(mazeLines);
        console.log("Number of tiles on at least one best path (Part 2): ".concat(part2Result));
    }
    catch (error) {
        console.error(error);
    }
}
main();
