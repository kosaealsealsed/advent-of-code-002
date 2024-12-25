import * as fs from "fs";

// Function to read the grid from the input file
function readGrid(filePath: string): string[] {
    try {
        const data = fs.readFileSync(filePath, "utf8");
        return data.split("\n").map(line => line.trim()).filter(line => line.length > 0);
    } catch (err) {
        console.error("Error reading file:", err);
        return [];
    }
}

// Function to check if the word exists in a given direction
function checkWord(
    grid: string[],
    x: number,
    y: number,
    dx: number,
    dy: number,
    word: string,
    rows: number,
    cols: number
): boolean {
    for (let i = 0; i < word.length; i++) {
        const nx = x + i * dx;
        const ny = y + i * dy;

        if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] !== word[i]) {
            return false;
        }
    }
    return true;
}

// Count occurrences of the word "XMAS" in all directions
function countXMAS(grid: string[]): number {
    const targetWord = "XMAS";
    const directions = [
        [0, 1],   // Right
        [1, 0],   // Down
        [1, 1],   // Diagonal-right-down
        [1, -1],  // Diagonal-left-down
        [0, -1],  // Left
        [-1, 0],  // Up
        [-1, -1], // Diagonal-left-up
        [-1, 1]   // Diagonal-right-up
    ];

    const rows = grid.length;
    const cols = grid[0].length;
    let count = 0;

    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            for (const [dx, dy] of directions) {
                if (checkWord(grid, r, c, dx, dy, targetWord, rows, cols)) {
                    count++;
                }
            }
        }
    }

    return count;
}

// Count all X-MAS patterns
function countAllXMASPatterns(grid: string[]): number {
    const rows = grid.length;
    const cols = grid[0].length;
    let count = 0;

    for (let r = 1; r < rows - 1; r++) {
        for (let c = 1; c < cols - 1; c++) {
            const center = grid[r][c];
            const topLeft = grid[r - 1][c - 1];
            const topRight = grid[r - 1][c + 1];
            const bottomLeft = grid[r + 1][c - 1];
            const bottomRight = grid[r + 1][c + 1];

            if (center === 'A') {
                // Pattern 1: M.S
                if (topLeft === 'M' && topRight === 'S' && bottomLeft === 'M' && bottomRight === 'S') count++;
                // Pattern 2: S.M
                if (topLeft === 'S' && topRight === 'M' && bottomLeft === 'S' && bottomRight === 'M') count++;
                // Pattern 3: M.M
                if (topLeft === 'M' && topRight === 'M' && bottomLeft === 'S' && bottomRight === 'S') count++;
                // Pattern 4: S.S
                if (topLeft === 'S' && topRight === 'S' && bottomLeft === 'M' && bottomRight === 'M') count++;
            }
        }
    }

    return count;
}

// Main function
function main() {
    const filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt";
    const grid = readGrid(filePath);

    if (grid.length === 0) {
        console.error("Error: Grid is empty or invalid.");
        return;
    }

    const xmasCount = countXMAS(grid);
    console.log(`Count of XMAS: ${xmasCount}`);

    const xmasPatterns = countAllXMASPatterns(grid);
    console.log(`Total X-MAS patterns: ${xmasPatterns}`);
}

// Run the main function
main();
