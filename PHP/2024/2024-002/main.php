<?php
// Function to check if a report is safe
function is_safe($report) {
    $differences = [];
    for ($i = 0; $i < count($report) - 1; $i++) {
        $differences[] = $report[$i + 1] - $report[$i];
    }
    
    $all_increasing = array_reduce($differences, function($carry, $diff) {
        return $carry && ($diff >= 1 && $diff <= 3);
    }, true);

    $all_decreasing = array_reduce($differences, function($carry, $diff) {
        return $carry && ($diff <= -1 && $diff >= -3);
    }, true);

    return $all_increasing || $all_decreasing;
}

// Function to check if a report is safe with the Problem Dampener
function is_safe_with_dampener($report) {
    if (is_safe($report)) {
        return true;
    }

    for ($i = 0; $i < count($report); $i++) {
        $modified_report = $report;
        array_splice($modified_report, $i, 1);
        if (is_safe($modified_report)) {
            return true;
        }
    }

    return false;
}

// Main function to process the input and count safe reports
function main() {
    $input_path = 'input.txt';

    // Read the input file
    $lines = file($input_path, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
    $reports = array_map(function($line) {
        return array_map('intval', preg_split('/\s+/', $line));
    }, $lines);

    // Count safe reports
    $safe_count = count(array_filter($reports, 'is_safe'));
    echo "Safe reports: $safe_count\n";

    // Count safe reports with the Problem Dampener
    $safe_with_dampener_count = count(array_filter($reports, 'is_safe_with_dampener'));
    echo "Safe reports with dampener: $safe_with_dampener_count\n";
}

// Run the main function
main();
