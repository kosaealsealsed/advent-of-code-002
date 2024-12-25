<?php

/**
 * Reads the grid map from a file.
 * @param string $filename The name of the file containing the grid map.
 * @return array A list of strings representing the grid.
 */
function readMap($filename) {
    return array_map('rtrim', file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES));
}

/**
 * Computes unique antinode positions using the pairwise method.
 * @param array $grid A list of strings representing the grid map.
 * @return array A set of unique antinode positions as strings.
 */
function computeAntinodesPairwise($grid) {
    $rows = count($grid);
    $cols = $rows > 0 ? strlen($grid[0]) : 0;

    // Group antennas by frequency
    $antennasByFreq = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $ch = $grid[$r][$c];
            if ($ch !== '.') {
                if (!isset($antennasByFreq[$ch])) {
                    $antennasByFreq[$ch] = [];
                }
                $antennasByFreq[$ch][] = [$r, $c];
            }
        }
    }

    $antinodes = [];

    foreach ($antennasByFreq as $freq => $coords) {
        $n = count($coords);
        if ($n < 2) continue;

        for ($i = 0; $i < $n; $i++) {
            list($rA, $cA) = $coords[$i];
            for ($j = $i + 1; $j < $n; $j++) {
                list($rB, $cB) = $coords[$j];

                // Compute P1 = 2B - A
                $p1_r = 2 * $rB - $rA;
                $p1_c = 2 * $cB - $cA;
                if ($p1_r >= 0 && $p1_r < $rows && $p1_c >= 0 && $p1_c < $cols) {
                    $antinodes["$p1_r,$p1_c"] = true;
                }

                // Compute P2 = 2A - B
                $p2_r = 2 * $rA - $rB;
                $p2_c = 2 * $cA - $cB;
                if ($p2_r >= 0 && $p2_r < $rows && $p2_c >= 0 && $p2_c < $cols) {
                    $antinodes["$p2_r,$p2_c"] = true;
                }
            }
        }
    }
    return array_keys($antinodes);
}

/**
 * Computes unique antinode positions using the line-drawing method.
 * @param array $grid A list of strings representing the grid map.
 * @return array A set of unique antinode positions as strings.
 */
function computeAntinodesLines($grid) {
    $rows = count($grid);
    $cols = $rows > 0 ? strlen($grid[0]) : 0;

    // Group antennas by frequency
    $antennasByFreq = [];
    for ($r = 0; $r < $rows; $r++) {
        for ($c = 0; $c < $cols; $c++) {
            $ch = $grid[$r][$c];
            if ($ch !== '.') {
                if (!isset($antennasByFreq[$ch])) {
                    $antennasByFreq[$ch] = [];
                }
                $antennasByFreq[$ch][] = [$r, $c];
            }
        }
    }

    $antinodes = [];

    foreach ($antennasByFreq as $freq => $coords) {
        $n = count($coords);
        if ($n < 2) continue;

        for ($i = 0; $i < $n; $i++) {
            list($rA, $cA) = $coords[$i];
            for ($j = $i + 1; $j < $n; $j++) {
                list($rB, $cB) = $coords[$j];
                addLinePoints($rA, $cA, $rB, $cB, $rows, $cols, $antinodes);
            }
        }
    }
    return array_keys($antinodes);
}

/**
 * Adds all points along a line between two antennas to the antinodes set.
 * @param int $rA Row of the first antenna.
 * @param int $cA Column of the first antenna.
 * @param int $rB Row of the second antenna.
 * @param int $cB Column of the second antenna.
 * @param int $rows Number of rows in the grid.
 * @param int $cols Number of columns in the grid.
 * @param array $antinodes Array to store unique antinode positions.
 */
function addLinePoints($rA, $cA, $rB, $cB, $rows, $cols, &$antinodes) {
    $dr = $rB - $rA;
    $dc = $cB - $cA;
    $g = gcd(abs($dr), abs($dc));
    $dr /= $g;
    $dc /= $g;

    // Add points in the forward direction
    $rP = $rA;
    $cP = $cA;
    while ($rP >= 0 && $rP < $rows && $cP >= 0 && $cP < $cols) {
        $antinodes["$rP,$cP"] = true;
        $rP += $dr;
        $cP += $dc;
    }

    // Add points in the backward direction
    $rP = $rA - $dr;
    $cP = $cA - $dc;
    while ($rP >= 0 && $rP < $rows && $cP >= 0 && $cP < $cols) {
        $antinodes["$rP,$cP"] = true;
        $rP -= $dr;
        $cP -= $dc;
    }
}

/**
 * Computes the greatest common divisor of two integers.
 * @param int $a First integer.
 * @param int $b Second integer.
 * @return int The greatest common divisor of a and b.
 */
function gcd($a, $b) {
    return $b === 0 ? $a : gcd($b, $a % $b);
}

/**
 * Formats the elapsed time in seconds with nanoseconds as a human-readable string.
 * @param float $seconds The elapsed time in seconds.
 * @return string The formatted time as a string.
 */
function formatTime($seconds) {
    return number_format($seconds, 9) . ' s';
}

/**
 * Main function to run the program.
 */
function main() {
    $filename = 'input.txt';
    $grid = readMap($filename);

    $startTime = microtime(true);

    // Part 1: Compute using Pairwise method
    $pairwiseStart = microtime(true);
    $pairwiseAntinodes = computeAntinodesPairwise($grid);
    $pairwiseEnd = microtime(true);
    echo "Pairwise: " . formatTime($pairwiseEnd - $pairwiseStart) . "\n";
    echo "Number of unique antinodes (Pairwise method): " . count($pairwiseAntinodes) . "\n";

    // Part 2: Compute using Line-drawing method
    $lineStart = microtime(true);
    $lineAntinodes = computeAntinodesLines($grid);
    $lineEnd = microtime(true);
    echo "Line-drawing: " . formatTime($lineEnd - $lineStart) . "\n";
    echo "Number of unique antinodes (Line-drawing method): " . count($lineAntinodes) . "\n";

    $endTime = microtime(true);
    echo "Overall: " . formatTime($endTime - $startTime) . "\n";
}

main();
