<?php

define('INPUT_FILE', 'input.txt');

function readMap($filename) {
    $grid = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    foreach ($lines as $line) {
        $row = array_map('intval', str_split(trim($line)));
        $grid[] = $row;
    }
    return $grid;
}

function neighbors($r, $c, $rows, $cols) {
    $directions = [
        [-1, 0], [1, 0], [0, -1], [0, 1]
    ];
    $result = [];
    foreach ($directions as [$dr, $dc]) {
        $nr = $r + $dr;
        $nc = $c + $dc;
        if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
            $result[] = [$nr, $nc];
        }
    }
    return $result;
}

function findTrailheadScores($grid) {
    $rows = count($grid);
    $cols = $rows > 0 ? count($grid[0]) : 0;

    $trailheads = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === 0) {
                $trailheads[] = [$r, $c];
            }
        }
    }

    $totalScore = 0;

    foreach ($trailheads as [$startR, $startC]) {
        $visited = [];
        $queue = [[$startR, $startC]];
        $visited["$startR,$startC"] = true;

        $reachableNines = [];

        while (!empty($queue)) {
            [$r, $c] = array_shift($queue);
            $currentHeight = $grid[$r][$c];

            if ($currentHeight === 9) {
                $reachableNines["$r,$c"] = true;
            } else {
                $nextHeight = $currentHeight + 1;
                foreach (neighbors($r, $c, $rows, $cols) as [$nr, $nc]) {
                    if (!isset($visited["$nr,$nc"]) && $grid[$nr][$nc] === $nextHeight) {
                        $visited["$nr,$nc"] = true;
                        $queue[] = [$nr, $nc];
                    }
                }
            }
        }

        $totalScore += count($reachableNines);
    }

    return $totalScore;
}

function countPaths($r, $c, $grid, &$dp, $rows, $cols) {
    if ($dp[$r][$c] !== -1) {
        return $dp[$r][$c];
    }

    $currentHeight = $grid[$r][$c];

    if ($currentHeight === 9) {
        $dp[$r][$c] = 1;
        return 1;
    }

    $totalPaths = 0;
    $nextHeight = $currentHeight + 1;
    foreach (neighbors($r, $c, $rows, $cols) as [$nr, $nc]) {
        if ($grid[$nr][$nc] === $nextHeight) {
            $totalPaths += countPaths($nr, $nc, $grid, $dp, $rows, $cols);
        }
    }

    $dp[$r][$c] = $totalPaths;
    return $totalPaths;
}

function calculateTotalRating($grid) {
    $rows = count($grid);
    $cols = $rows > 0 ? count($grid[0]) : 0;

    $trailheads = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($grid[$r][$c] === 0) {
                $trailheads[] = [$r, $c];
            }
        }
    }

    $dp = array_fill(0, $rows, array_fill(0, $cols, -1));

    $totalRating = 0;
    foreach ($trailheads as [$tr, $tc]) {
        $totalRating += countPaths($tr, $tc, $grid, $dp, $rows, $cols);
    }

    return $totalRating;
}

function main() {
    $startTimePart1 = microtime(true);
    $grid = readMap(INPUT_FILE);
    $totalScore = findTrailheadScores($grid);
    $endTimePart1 = microtime(true);
    echo "Part 1 Result: $totalScore\n";
    printf("Time taken for Part 1: %.9f s\n", $endTimePart1 - $startTimePart1);

    $startTimePart2 = microtime(true);
    $totalRating = calculateTotalRating($grid);
    $endTimePart2 = microtime(true);
    echo "Part 2 Result: $totalRating\n";
    printf("Time taken for Part 2: %.9f s\n", $endTimePart2 - $startTimePart2);
}

main();
