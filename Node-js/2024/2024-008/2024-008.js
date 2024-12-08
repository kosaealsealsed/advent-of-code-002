const fs = require('fs');

/**
 * Reads the grid map from a file.
 * @param {string} filename The name of the file containing the grid map.
 * @returns {string[]} A list of strings representing the grid.
 */
function readMap(filename) {
    return fs.readFileSync(filename, 'utf-8').trim().split('\n');
}

/**
 * Computes unique antinode positions using the pairwise method.
 * @param {string[]} grid A list of strings representing the grid map.
 * @returns {Set<string>} A set of unique antinode positions as strings.
 */
function computeAntinodesPairwise(grid) {
    const rows = grid.length;
    const cols = rows > 0 ? grid[0].length : 0;

    // Group antennas by frequency
    const antennasByFreq = new Map();
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            const ch = grid[r][c];
            if (ch !== '.') {
                if (!antennasByFreq.has(ch)) {
                    antennasByFreq.set(ch, []);
                }
                antennasByFreq.get(ch).push([r, c]);
            }
        }
    }

    const antinodes = new Set();

    for (const [freq, coords] of antennasByFreq) {
        const n = coords.length;
        if (n < 2) continue;

        for (let i = 0; i < n; i++) {
            const [rA, cA] = coords[i];
            for (let j = i + 1; j < n; j++) {
                const [rB, cB] = coords[j];

                // Compute P1 = 2B - A
                const p1_r = 2 * rB - rA;
                const p1_c = 2 * cB - cA;
                if (p1_r >= 0 && p1_r < rows && p1_c >= 0 && p1_c < cols) {
                    antinodes.add(`${p1_r},${p1_c}`);
                }

                // Compute P2 = 2A - B
                const p2_r = 2 * rA - rB;
                const p2_c = 2 * cA - cB;
                if (p2_r >= 0 && p2_r < rows && p2_c >= 0 && p2_c < cols) {
                    antinodes.add(`${p2_r},${p2_c}`);
                }
            }
        }
    }
    return antinodes;
}

/**
 * Computes unique antinode positions using the line-drawing method.
 * @param {string[]} grid A list of strings representing the grid map.
 * @returns {Set<string>} A set of unique antinode positions as strings.
 */
function computeAntinodesLines(grid) {
    const rows = grid.length;
    const cols = rows > 0 ? grid[0].length : 0;

    // Group antennas by frequency
    const antennasByFreq = new Map();
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            const ch = grid[r][c];
            if (ch !== '.') {
                if (!antennasByFreq.has(ch)) {
                    antennasByFreq.set(ch, []);
                }
                antennasByFreq.get(ch).push([r, c]);
            }
        }
    }

    const antinodes = new Set();

    for (const [freq, coords] of antennasByFreq) {
        const n = coords.length;
        if (n < 2) continue;

        for (let i = 0; i < n; i++) {
            const [rA, cA] = coords[i];
            for (let j = i + 1; j < n; j++) {
                const [rB, cB] = coords[j];
                addLinePoints(rA, cA, rB, cB, rows, cols, antinodes);
            }
        }
    }
    return antinodes;
}

/**
 * Adds all points along a line between two antennas to the antinodes set.
 * @param {number} rA Row of the first antenna.
 * @param {number} cA Column of the first antenna.
 * @param {number} rB Row of the second antenna.
 * @param {number} cB Column of the second antenna.
 * @param {number} rows Number of rows in the grid.
 * @param {number} cols Number of columns in the grid.
 * @param {Set<string>} antinodes Set to store unique antinode positions.
 */
function addLinePoints(rA, cA, rB, cB, rows, cols, antinodes) {
    let dr = rB - rA;
    let dc = cB - cA;
    const g = gcd(Math.abs(dr), Math.abs(dc));
    dr /= g;
    dc /= g;

    // Add points in the forward direction
    let rP = rA, cP = cA;
    while (rP >= 0 && rP < rows && cP >= 0 && cP < cols) {
        antinodes.add(`${rP},${cP}`);
        rP += dr;
        cP += dc;
    }

    // Add points in the backward direction
    rP = rA - dr;
    cP = cA - dc;
    while (rP >= 0 && rP < rows && cP >= 0 && cP < cols) {
        antinodes.add(`${rP},${cP}`);
        rP -= dr;
        cP -= dc;
    }
}

/**
 * Computes the greatest common divisor of two integers.
 * @param {number} a First integer.
 * @param {number} b Second integer.
 * @returns {number} The greatest common divisor of a and b.
 */
function gcd(a, b) {
    return b === 0 ? a : gcd(b, a % b);
}

/**
 * Formats the elapsed time in seconds with nanoseconds as a human-readable string.
 * @param {number} nanos The elapsed time in nanoseconds.
 * @returns {string} The formatted time as a string.
 */
function formatTime(nanos) {
    return `${(nanos / 1e9).toFixed(9)} s`;
}

/**
 * Main function to run the program.
 */
function main() {
    const filename = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-008\\input.txt';
    const grid = readMap(filename);

    console.time("Overall");

    // Part 1: Compute using Pairwise method
    console.time("Pairwise");
    const pairwiseAntinodes = computeAntinodesPairwise(grid);
    console.timeEnd("Pairwise");
    console.log("Number of unique antinodes (Pairwise method):", pairwiseAntinodes.size);

    // Part 2: Compute using Line-drawing method
    console.time("Line-drawing");
    const lineAntinodes = computeAntinodesLines(grid);
    console.timeEnd("Line-drawing");
    console.log("Number of unique antinodes (Line-drawing method):", lineAntinodes.size);

    console.timeEnd("Overall");
}

main();
