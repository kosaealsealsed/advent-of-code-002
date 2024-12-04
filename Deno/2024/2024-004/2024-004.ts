// Read the grid from the input file
const readGrid = async (filePath) => {
  const data = await Deno.readTextFile(filePath);
  return data.trim().split("\n").map((line) => line.split(""));
};

// Define the target word and movement directions
const TARGET_WORD = "XMAS";
const DIRECTIONS = [
  [0, 1],   // Right
  [1, 0],   // Down
  [1, 1],   // Diagonal-right-down
  [1, -1],  // Diagonal-left-down
  [0, -1],  // Left
  [-1, 0],  // Up
  [-1, -1], // Diagonal-left-up
  [-1, 1],  // Diagonal-right-up
];

// Function to check if the word exists in a given direction
const checkWord = (grid, word, x, y, dx, dy) => {
  const rows = grid.length;
  const cols = grid[0].length;
  for (let i = 0; i < word.length; i++) {
    const nx = x + i * dx;
    const ny = y + i * dy;
    if (nx < 0 || ny < 0 || nx >= rows || ny >= cols || grid[nx][ny] !== word[i]) {
      return false;
    }
  }
  return true;
};

// Function to count all occurrences of the target word
const countOccurrences = (grid, word) => {
  let count = 0;
  const rows = grid.length;
  const cols = grid[0].length;
  for (let x = 0; x < rows; x++) {
    for (let y = 0; y < cols; y++) {
      for (const [dx, dy] of DIRECTIONS) {
        if (checkWord(grid, word, x, y, dx, dy)) {
          count++;
        }
      }
    }
  }
  return count;
};

// Function to count all X-MAS patterns
const countXmasPatterns = (grid) => {
  let count = 0;
  const rows = grid.length;
  const cols = grid[0].length;
  for (let x = 1; x < rows - 1; x++) {
    for (let y = 1; y < cols - 1; y++) {
      const center = grid[x][y];
      const topLeft = grid[x - 1][y - 1];
      const topRight = grid[x - 1][y + 1];
      const bottomLeft = grid[x + 1][y - 1];
      const bottomRight = grid[x + 1][y + 1];

      if (
        center === "A" &&
        (
          (topLeft === "M" && topRight === "S" && bottomLeft === "M" && bottomRight === "S") ||
          (topLeft === "S" && topRight === "M" && bottomLeft === "S" && bottomRight === "M") ||
          (topLeft === "M" && topRight === "M" && bottomLeft === "S" && bottomRight === "S") ||
          (topLeft === "S" && topRight === "S" && bottomLeft === "M" && bottomRight === "M")
        )
      ) {
        count++;
      }
    }
  }
  return count;
};

// Main execution
const main = async () => {
  const filePath = "/uploads/input.txt";
  const grid = await readGrid(filePath);
  console.log("Count of XMAS occurrences:", countOccurrences(grid, TARGET_WORD));
  console.log("Total XMAS patterns:", countXmasPatterns(grid));
};

main();
