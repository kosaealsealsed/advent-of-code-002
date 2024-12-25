<?php
function parseInput($filename) {
    return array_map('rtrim', file($filename));
}

function solveMazePart1($mazeLines) {
    // Directions: 0=East, 1=South, 2=West, 3=North
    $directions = [
        [0, 1],   // East
        [1, 0],   // South
        [0, -1],  // West
        [-1, 0]   // North
    ];

    $rows = count($mazeLines);
    $cols = strlen($mazeLines[0]);

    // Find S and E
    $start = null;
    $end = null;
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($mazeLines[$r][$c] === 'S') {
                $start = [$r, $c];
            } elseif ($mazeLines[$r][$c] === 'E') {
                $end = [$r, $c];
            }
        }
    }
    if (!$start || !$end) {
        throw new Exception("Could not find 'S' or 'E' in the maze.");
    }

    $INF = PHP_INT_MAX;
    $dist = [];
    for ($i = 0; $i < $rows; $i++) {
        $dist[$i] = array_fill(0, $cols, array_fill(0, 4, $INF));
    }

    $startDir = 0; // facing East
    $dist[$start[0]][$start[1]][$startDir] = 0;

    $pq = new SplPriorityQueue();
    $pq->insert([$start[0], $start[1], $startDir], 0);

    $visited = [];

    while (!$pq->isEmpty()) {
        [$r, $c, $d] = $pq->extract();

        if ($r === $end[0] && $c === $end[1]) {
            return $dist[$r][$c][$d]; // Minimum cost to reach E
        }

        $visitedKey = "$r,$c,$d";
        if (isset($visited[$visitedKey])) {
            continue;
        }
        $visited[$visitedKey] = true;

        $cost = $dist[$r][$c][$d];

        // 1) Move forward
        [$dr, $dc] = $directions[$d];
        $nr = $r + $dr;
        $nc = $c + $dc;
        if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $mazeLines[$nr][$nc] !== '#') {
            $newCost = $cost + 1;
            if ($newCost < $dist[$nr][$nc][$d]) {
                $dist[$nr][$nc][$d] = $newCost;
                $pq->insert([$nr, $nc, $d], -$newCost);
            }
        }

        // 2) Turn left
        $leftDir = ($d - 1 + 4) % 4;
        $newCost = $cost + 1000;
        if ($newCost < $dist[$r][$c][$leftDir]) {
            $dist[$r][$c][$leftDir] = $newCost;
            $pq->insert([$r, $c, $leftDir], -$newCost);
        }

        // 3) Turn right
        $rightDir = ($d + 1) % 4;
        $newCost = $cost + 1000;
        if ($newCost < $dist[$r][$c][$rightDir]) {
            $dist[$r][$c][$rightDir] = $newCost;
            $pq->insert([$r, $c, $rightDir], -$newCost);
        }
    }

    return null; // No path found
}

function solvePart2($mazeLines) {
    $rows = count($mazeLines);
    $cols = strlen($mazeLines[0]);

    // Directions: 0=East, 1=South, 2=West, 3=North
    $directions = [
        [0, 1],   // East
        [1, 0],   // South
        [0, -1],  // West
        [-1, 0]   // North
    ];

    $start = null;
    $end = null;

    // Identify S and E
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $ch = $mazeLines[$r][$c];
            if ($ch === 'S') {
                $start = [$r, $c];
            } elseif ($ch === 'E') {
                $end = [$r, $c];
            }
        }
    }

    if (!$start || !$end) {
        throw new Exception("Could not find 'S' or 'E' in the maze.");
    }

    $INF = PHP_INT_MAX;
    $dist = [];
    for ($i = 0; $i < $rows; $i++) {
        $dist[$i] = array_fill(0, $cols, array_fill(0, 4, $INF));
    }

    $startDir = 0; // The reindeer start facing East = 0
    $dist[$start[0]][$start[1]][$startDir] = 0;

    $pq = new SplPriorityQueue();
    $pq->insert([$start[0], $start[1], $startDir], 0);
    $visited = [];

    while (!$pq->isEmpty()) {
        [$r, $c, $d] = $pq->extract();

        $visitedKey = "$r,$c,$d";
        if (isset($visited[$visitedKey])) {
            continue;
        }
        $visited[$visitedKey] = true;

        $cost = $dist[$r][$c][$d];

        // Explore moves:
        // 1) Move forward
        [$dr, $dc] = $directions[$d];
        $nr = $r + $dr;
        $nc = $c + $dc;
        if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols && $mazeLines[$nr][$nc] !== '#') {
            $newCost = $cost + 1;
            if ($newCost < $dist[$nr][$nc][$d]) {
                $dist[$nr][$nc][$d] = $newCost;
                $pq->insert([$nr, $nc, $d], -$newCost);
            }
        }

        // 2) Turn left
        $leftDir = ($d - 1 + 4) % 4;
        $newCost = $cost + 1000;
        if ($newCost < $dist[$r][$c][$leftDir]) {
            $dist[$r][$c][$leftDir] = $newCost;
            $pq->insert([$r, $c, $leftDir], -$newCost);
        }

        // 3) Turn right
        $rightDir = ($d + 1) % 4;
        $newCost = $cost + 1000;
        if ($newCost < $dist[$r][$c][$rightDir]) {
            $dist[$r][$c][$rightDir] = $newCost;
            $pq->insert([$r, $c, $rightDir], -$newCost);
        }
    }

    $minCostEnd = $INF;
    for ($d = 0; $d < 4; $d++) {
        $minCostEnd = min($minCostEnd, $dist[$end[0]][$end[1]][$d]);
    }

    if ($minCostEnd === $INF) {
        return 0; // No path
    }

    $onBestPath = array_fill(0, $rows, array_fill(0, $cols, false));
    $queue = new SplQueue();
    for ($d = 0; $d < 4; $d++) {
        if ($dist[$end[0]][$end[1]][$d] === $minCostEnd) {
            $queue->enqueue([$end[0], $end[1], $d]);
        }
    }

    $visitedRev = [];
    while (!$queue->isEmpty()) {
        [$r, $c, $d] = $queue->dequeue();
        $onBestPath[$r][$c] = true;

        $costHere = $dist[$r][$c][$d];

        // (A) Predecessor by moving forward
        [$dr, $dc] = $directions[$d];
        $rPrev = $r - $dr;
        $cPrev = $c - $dc;
        if ($rPrev >= 0 && $rPrev < $rows && $cPrev >= 0 && $cPrev < $cols && $mazeLines[$rPrev][$cPrev] !== '#') {
            if ($dist[$rPrev][$cPrev][$d] === $costHere - 1) {
                $visitedKey = "$rPrev,$cPrev,$d";
                if (!isset($visitedRev[$visitedKey])) {
                    $visitedRev[$visitedKey] = true;
                    $queue->enqueue([$rPrev, $cPrev, $d]);
                }
            }
        }

        // (B) Predecessors by turning
        foreach ([($d - 1 + 4) % 4, ($d + 1) % 4] as $dPre) {
            if ($dist[$r][$c][$dPre] === $costHere - 1000) {
                $visitedKey = "$r,$c,$dPre";
                if (!isset($visitedRev[$visitedKey])) {
                    $visitedRev[$visitedKey] = true;
                    $queue->enqueue([$r, $c, $dPre]);
                }
            }
        }
    }

    $result = 0;
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            if ($onBestPath[$r][$c]) {
                $result++;
            }
        }
    }

    return $result;
}

function main() {
    $mazeLines = parseInput("input.txt");

    // Part 1
    $answer1 = solveMazePart1($mazeLines);
    if ($answer1 === null) {
        echo "No path found in Part 1!\n";
    } else {
        echo "Lowest possible score in Part 1: $answer1\n";
    }

    // Part 2
    $answer2 = solvePart2($mazeLines);
    echo "Number of tiles on at least one best path in Part 2: $answer2\n";
}

main();
?>
