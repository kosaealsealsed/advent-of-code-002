<?php

class GuardPatrolLoopDetector
{
    // Direction mappings
    private const DIRECTION_MAP = [
        '^' => 0,
        '>' => 1,
        'v' => 2,
        '<' => 3
    ];

    private const DIRECTION_OFFSETS = [
        [-1, 0], // Up
        [0, 1],  // Right
        [1, 0],  // Down
        [0, -1]  // Left
    ];

    public static function main()
    {
        $filePath = "/uploads/input.txt"; // Specify the path to your input file here
        try {
            // Part 1: Count distinct positions visited without obstructions
            $distinctPositions = self::countDistinctPositionsVisited($filePath);
            echo "Number of distinct positions visited: " . $distinctPositions . PHP_EOL;

            // Part 2: Detect loops with obstructions and measure execution times
            self::countObstructionPositions($filePath);
        } catch (Exception $e) {
            fwrite(STDERR, "Error: " . $e->getMessage() . PHP_EOL);
        }
    }

    /**
     * Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
     *
     * @param string $filePath Path to the input file.
     * @return int Number of distinct positions visited.
     * @throws Exception If the file cannot be read.
     */
    private static function countDistinctPositionsVisited(string $filePath): int
    {
        // Parse the grid
        $grid = self::parseGrid($filePath);

        // Find the guard's starting position and direction
        [$guardPos, $guardDir] = self::findGuard($grid);

        // Initialize visited positions set
        $visitedPositions = [];
        $visitedPositions[$guardPos->__toString()] = true;

        // Simulate the guard's movement
        while (true) {
            $dr = self::DIRECTION_OFFSETS[$guardDir][0];
            $dc = self::DIRECTION_OFFSETS[$guardDir][1];
            $newR = $guardPos->row + $dr;
            $newC = $guardPos->col + $dc;

            // Check boundaries
            if ($newR < 0 || $newR >= count($grid) || $newC < 0 || $newC >= count($grid[0])) {
                break; // Guard exits the mapped area
            }

            if ($grid[$newR][$newC] === '#') {
                // Turn right if obstacle ahead
                $guardDir = ($guardDir + 1) % 4;
            } else {
                // Move forward
                $guardPos = new Position($newR, $newC);
                $visitedPositions[$guardPos->__toString()] = true;
            }
        }

        // Number of distinct positions visited
        return count($visitedPositions);
    }

    /**
     * Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
     * Also measures and prints execution times.
     *
     * @param string $filePath Path to the input file.
     * @throws Exception If the file cannot be read.
     */
    private static function countObstructionPositions(string $filePath): void
    {
        // Start total timing
        $totalStartTime = microtime(true);

        // Parse the grid
        $grid = self::parseGrid($filePath);

        // Find the guard's starting position and direction
        [$guardPos, $guardDir] = self::findGuard($grid);

        // Time to find obstruction positions
        $obstructionStartTime = microtime(true);
        $possibleObstructions = self::getPossibleObstructions($grid, $guardPos);
        $obstructionEndTime = microtime(true);
        $obstructionTime = $obstructionEndTime - $obstructionStartTime;

        // Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
        echo "time, denominator" . PHP_EOL;
        printf("%.9f %d\n", $obstructionTime, count($possibleObstructions));

        // Print header for batches
        echo "batch, batch time, cumulative time" . PHP_EOL;

        // Initialize loop counter
        $loopCount = 0;
        $total = count($possibleObstructions);

        // Initialize timing for batches
        $batchSize = 1000;
        $batchStartTime = microtime(true);
        $cumulativeTime = $obstructionTime; // cumulative_time includes obstruction_time

        foreach ($possibleObstructions as $idx => $obstruction) {
            $grid[$obstruction->row][$obstruction->col] = '#'; // Place obstruction

            if (self::simulateMovement($grid, $guardPos, $guardDir)) {
                $loopCount++; // Found a position that causes a loop
            }

            $grid[$obstruction->row][$obstruction->col] = '.'; // Remove obstruction

            // Check if batch size is reached or it's the last position
            if ((($idx + 1) % $batchSize === 0) || (($idx + 1) === $total)) {
                $batchEndTime = microtime(true);
                $batchTime = $batchEndTime - $batchStartTime;
                $cumulativeTime += $batchTime;
                printf("%d %.9f %.9f\n", $idx + 1, $batchTime, $cumulativeTime);
                $batchStartTime = microtime(true); // Reset batch start time
            }
        }

        // End total timing
        $totalEndTime = microtime(true);
        $totalTime = $totalEndTime - $totalStartTime; // Total time from start to end

        // Print final answer header and line: [answer] [answer_time]
        echo "answer, answer time" . PHP_EOL;
        printf("%d %.9f\n", $loopCount, $totalTime);
    }

    /**
     * Parses the grid from the given file.
     *
     * @param string $filePath Path to the input file.
     * @return array 2D character array representing the grid.
     * @throws Exception If the file cannot be read or grid is invalid.
     */
    private static function parseGrid(string $filePath): array
    {
        if (!file_exists($filePath)) {
            throw new Exception("File not found: " . $filePath);
        }

        $gridList = [];
        $file = fopen($filePath, 'r');
        if (!$file) {
            throw new Exception("Unable to open file: " . $filePath);
        }

        while (($line = fgets($file)) !== false) {
            $trimmedLine = trim($line);
            if ($trimmedLine !== '') {
                $gridList[] = mb_str_split($trimmedLine);
            }
        }
        fclose($file);

        if (empty($gridList)) {
            throw new Exception("The grid is empty.");
        }

        // Ensure all rows have the same length
        $cols = count($gridList[0]);
        foreach ($gridList as $row) {
            if (count($row) !== $cols) {
                throw new Exception("Inconsistent row lengths in the grid.");
            }
        }

        return $gridList;
    }

    /**
     * Finds the guard's starting position and direction.
     *
     * @param array $grid 2D character array representing the grid.
     * @return array A tuple containing the starting Position and direction.
     * @throws Exception If the guard is not found in the grid.
     */
    private static function findGuard(array &$grid): array
    {
        foreach ($grid as $r => $row) {
            foreach ($row as $c => $cell) {
                if (array_key_exists($cell, self::DIRECTION_MAP)) {
                    $guardPos = new Position($r, $c);
                    $guardDir = self::DIRECTION_MAP[$cell];
                    $grid[$r][$c] = '.'; // Clear the starting position
                    return [$guardPos, $guardDir];
                }
            }
        }
        throw new Exception("Guard not found in the grid.");
    }

    /**
     * Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
     *
     * @param array $grid     2D character array representing the grid.
     * @param Position $guardPos The starting position of the guard.
     * @return array List of possible obstruction Positions.
     */
    private static function getPossibleObstructions(array $grid, Position $guardPos): array
    {
        $possible = [];
        foreach ($grid as $r => $row) {
            foreach ($row as $c => $cell) {
                if (!(($r === $guardPos->row) && ($c === $guardPos->col)) && $cell === '.') {
                    $possible[] = new Position($r, $c);
                }
            }
        }
        return $possible;
    }

    /**
     * Simulates the guard's movement on the grid.
     *
     * @param array $grid     2D character array representing the grid.
     * @param Position $startPos Starting position of the guard.
     * @param int $startDir Starting direction of the guard.
     * @return bool True if a loop is detected, False if the guard exits the grid.
     */
    private static function simulateMovement(array $grid, Position $startPos, int $startDir): bool
    {
        $visitedStates = [];

        $r = $startPos->row;
        $c = $startPos->col;
        $direction = $startDir;

        while (true) {
            $currentState = new State($r, $c, $direction);
            $stateKey = $currentState->__toString();
            if (isset($visitedStates[$stateKey])) {
                return true; // Loop detected
            }
            $visitedStates[$stateKey] = true;

            $dr = self::DIRECTION_OFFSETS[$direction][0];
            $dc = self::DIRECTION_OFFSETS[$direction][1];
            $newR = $r + $dr;
            $newC = $c + $dc;

            // Check boundaries
            if ($newR < 0 || $newR >= count($grid) || $newC < 0 || $newC >= count($grid[0])) {
                return false; // Guard exits the grid
            }

            if ($grid[$newR][$newC] === '#') {
                // Turn right if obstacle ahead
                $direction = ($direction + 1) % 4;
            } else {
                // Move forward
                $r = $newR;
                $c = $newC;
            }
        }
    }
}

/**
 * Helper class to represent a position in the grid.
 */
class Position
{
    public int $row;
    public int $col;

    public function __construct(int $r, int $c)
    {
        $this->row = $r;
        $this->col = $c;
    }

    public function __toString(): string
    {
        return "{$this->row},{$this->col}";
    }
}

/**
 * Helper class to represent a state (position and direction) of the guard.
 */
class State
{
    public int $row;
    public int $col;
    public int $direction;

    public function __construct(int $r, int $c, int $dir)
    {
        $this->row = $r;
        $this->col = $c;
        $this->direction = $dir;
    }

    public function __toString(): string
    {
        return "{$this->row},{$this->col},{$this->direction}";
    }
}

// Execute the main function
GuardPatrolLoopDetector::main();
