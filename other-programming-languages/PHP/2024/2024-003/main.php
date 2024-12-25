<?php

function sumMulOperations($corruptedMemory) {
    // Regex pattern for mul(X,Y)
    $pattern = '/mul\((\d{1,3}),(\d{1,3})\)/';
    $total = 0;

    // Find all matches
    preg_match_all($pattern, $corruptedMemory, $matches, PREG_SET_ORDER);

    // Calculate the sum of products
    foreach ($matches as $match) {
        $num1 = intval($match[1]);
        $num2 = intval($match[2]);
        $total += $num1 * $num2;
    }

    return $total;
}

function sumEnabledMulOperations($corruptedMemory) {
    // Regex pattern for do(), don't(), and mul(X,Y)
    $pattern = '/(do\(\)|don\'t\(\)|mul\((\d{1,3}),(\d{1,3})\))/';
    $totalSum = 0;
    $mulEnabled = true; // mul instructions are enabled at the start

    // Find all matches
    preg_match_all($pattern, $corruptedMemory, $matches, PREG_SET_ORDER);

    foreach ($matches as $match) {
        $fullMatch = $match[1];

        if ($fullMatch === "do()") {
            $mulEnabled = true;
        } elseif ($fullMatch === "don't()") {
            $mulEnabled = false;
        } elseif (isset($match[2]) && isset($match[3])) {
            // This is a mul(X,Y) instruction
            if ($mulEnabled) {
                $num1 = intval($match[2]);
                $num2 = intval($match[3]);
                $totalSum += $num1 * $num2;
            }
        }
    }

    return $totalSum;
}

// Read the corrupted memory from 'input.txt'
$filePath = 'input.txt';
$corruptedMemory = file_get_contents($filePath);

// Calculate the results
$totalSumAllMulOperations = sumMulOperations($corruptedMemory);
$totalSumEnabledMulOperations = sumEnabledMulOperations($corruptedMemory);

// Output the results
echo "The sum of all mul operations is: $totalSumAllMulOperations\n";
echo "The sum of enabled mul operations is: $totalSumEnabledMulOperations\n";

?>
