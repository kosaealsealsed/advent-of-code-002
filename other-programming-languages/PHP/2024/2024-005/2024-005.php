<?php

// Reading the file content
$filePath = 'input.txt';
$content = file_get_contents($filePath);

// Splitting content into rules and updates
list($rulesSection, $updatesSection) = explode("\n\n", trim($content));

$rules = array_map(function ($line) {
    return array_map('intval', explode('|', $line));
}, explode("\n", $rulesSection));

$updates = array_map(function ($line) {
    return array_map('intval', explode(',', $line));
}, explode("\n", $updatesSection));

// Helper function to check if an update follows the rules
function isUpdateOrdered($update, $rules) {
    $indexMap = array_flip($update);
    foreach ($rules as list($x, $y)) {
        if (isset($indexMap[$x], $indexMap[$y]) && $indexMap[$x] > $indexMap[$y]) {
            return false;
        }
    }
    return true;
}

// Identify correctly ordered updates and their middle page numbers
$correctUpdates = [];
$middlePages = [];

foreach ($updates as $update) {
    if (isUpdateOrdered($update, $rules)) {
        $correctUpdates[] = $update;
        $middlePages[] = $update[intval(floor(count($update) / 2))];
    }
}

// Calculate the sum of middle pages
$sumMiddlePages = array_sum($middlePages);
echo "Sum of middle pages for correctly ordered updates: $sumMiddlePages\n";

// Helper function to sort an update according to the rules
function topologicalSort($update, $rules) {
    $graph = [];
    $inDegree = [];
    $nodes = array_flip($update);

    foreach ($rules as list($x, $y)) {
        if (isset($nodes[$x], $nodes[$y])) {
            $graph[$x][] = $y;
            $inDegree[$y] = ($inDegree[$y] ?? 0) + 1;
            $inDegree[$x] = $inDegree[$x] ?? 0;
        }
    }

    $queue = [];
    foreach ($nodes as $node => $_) {
        if (($inDegree[$node] ?? 0) === 0) {
            $queue[] = $node;
        }
    }

    $sortedUpdate = [];
    while (!empty($queue)) {
        $current = array_shift($queue);
        $sortedUpdate[] = $current;

        foreach ($graph[$current] ?? [] as $neighbor) {
            $inDegree[$neighbor]--;
            if ($inDegree[$neighbor] === 0) {
                $queue[] = $neighbor;
            }
        }
    }

    return $sortedUpdate;
}

// Correct the incorrectly ordered updates and find their middle page numbers
$incorrectUpdates = [];
$incorrectMiddlePages = [];

foreach ($updates as $update) {
    if (!isUpdateOrdered($update, $rules)) {
        $correctedUpdate = topologicalSort($update, $rules);
        $incorrectUpdates[] = $correctedUpdate;
        $incorrectMiddlePages[] = $correctedUpdate[intval(floor(count($correctedUpdate) / 2))];
    }
}

// Calculate the sum of middle pages for corrected updates
$sumIncorrectMiddlePages = array_sum($incorrectMiddlePages);
echo "Sum of middle pages for corrected updates: $sumIncorrectMiddlePages\n";

?>
