<?php
// Define the file path
$filePath = 'input.txt';

// Check if the file exists
if (!file_exists($filePath)) {
    die("File not found: " . $filePath);
}

// Load the file content into an array
$fileContent = file($filePath, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

// Initialize the left and right lists
$leftList = [];
$rightList = [];

// Parse the file content
foreach ($fileContent as $line) {
    $columns = preg_split('/\s+/', trim($line));
    if (count($columns) >= 2) {
        $leftList[] = (int)$columns[0];
        $rightList[] = (int)$columns[1];
    }
}

// Count occurrences in the right list
$rightListCounts = array_count_values($rightList);

// Calculate the similarity score
$similarityScore = 0;
foreach ($leftList as $left) {
    $similarityScore += $left * ($rightListCounts[$left] ?? 0);
}

// Output the result
echo "Similarity Score: " . $similarityScore;
?>
