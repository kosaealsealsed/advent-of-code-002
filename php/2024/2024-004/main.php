<?php

// Function to read the grid from the input file
function readGrid($filePath) {
    if (!file_exists($filePath)) {
        die("Error: File not found.\n");
    }

    $lines = file($filePath, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    return array_map('trim', $lines);
}

// Function to check if the word exists in a given direction
function checkWord($grid, $x, $y, $dx, $dy, $word, $rows, $cols) {
    $wordLength = strlen($word);

    for ($i = 0; $i < $wordLength; $i++) {
        $nx = $x + $i * $dx;
        $ny = $y + $i * $dy;

        if ($nx < 0 || $ny < 0 || $nx >= $rows || $ny >= $cols || $grid[$nx][$ny] !== $word[$i]) {
            return false;
        }
    }
    return true;
}

// Count occurrences of the word "XMAS" in all directions
function countXMAS($grid) {
    $targetWord = "XMAS";
    $directions = [
        [0, 1],   // Right
        [1, 0],   // Down
        [1, 1],   // Diagonal-right-down
        [1, -1],  // Diagonal-left-down
        [0, -1],  // Left
        [-1, 0],  // Up
        [-1, -1], // Diagonal-left-up
        [-1, 1]   // Diagonal-right-up
    ];

    $rows = count($grid);
    $cols = strlen($grid[0]);
    $count = 0;

    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            foreach ($directions as [$dx, $dy]) {
                if (checkWord($grid, $r, $c, $dx, $dy, $targetWord, $rows, $cols)) {
                    $count++;
                }
            }
        }
    }

    return $count;
}

// Count all X-MAS patterns
function countAllXMASPatterns($grid) {
    $rows = count($grid);
    $cols = strlen($grid[0]);
    $count = 0;

    for ($r = 1; $r < $rows - 1; $r++) {
        for ($c = 1; $c < $cols - 1; $c++) {
            $center = $grid[$r][$c];
            $topLeft = $grid[$r - 1][$c - 1];
            $topRight = $grid[$r - 1][$c + 1];
            $bottomLeft = $grid[$r + 1][$c - 1];
            $bottomRight = $grid[$r + 1][$c + 1];

            if ($center === 'A') {
                // Pattern 1: M.S
                if ($topLeft === 'M' && $topRight === 'S' && $bottomLeft === 'M' && $bottomRight === 'S') $count++;
                // Pattern 2: S.M
                if ($topLeft === 'S' && $topRight === 'M' && $bottomLeft === 'S' && $bottomRight === 'M') $count++;
                // Pattern 3: M.M
                if ($topLeft === 'M' && $topRight === 'M' && $bottomLeft === 'S' && $bottomRight === 'S') $count++;
                // Pattern 4: S.S
                if ($topLeft === 'S' && $topRight === 'S' && $bottomLeft === 'M' && $bottomRight === 'M') $count++;
            }
        }
    }

    return $count;
}

// Main function
function main() {
    $filePath = "input.txt";
    $grid = readGrid($filePath);

    if (empty($grid)) {
        echo "Error: Grid is empty or invalid.\n";
        return;
    }

    $xmasCount = countXMAS($grid);
    echo "Count of XMAS: $xmasCount\n";

    $xmasPatterns = countAllXMASPatterns($grid);
    echo "Total X-MAS patterns: $xmasPatterns\n";
}

// Run the main function
main();

?>
