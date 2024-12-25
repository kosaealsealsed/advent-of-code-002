// Function to read the grid from the input file
function readGrid(filePath) {
    try {
        // SpiderMonkey doesn't have 'fs', so use an environment-provided method like `read()` if available
        const data = read(filePath); // `read` is specific to some SpiderMonkey shells
        return data.split('\n').filter(line => line.trim() !== '');
    } catch (err) {
        print("Error reading file: " + err); // Use `print` instead of `console.log` in SpiderMonkey
        return [];
    }
}

// Function to check if the word exists in a given direction
function checkWord(grid, x, y, dx, dy, word, rows, cols) {
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
function countXMAS(grid) {
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
function countAllXMASPatterns(grid) {
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
    const filePath = '/uploads/input.txt';
    const grid = readGrid(filePath).map(line => line.trim());

    if (grid.length === 0) {
        print("Error: Grid is empty or invalid.");
        return;
    }

    const xmasCount = countXMAS(grid);
    print(`Count of XMAS: ${xmasCount}`);

    const xmasPatterns = countAllXMASPatterns(grid);
    print(`Total X-MAS patterns: ${xmasPatterns}`);
}

// Run the main function
main();
