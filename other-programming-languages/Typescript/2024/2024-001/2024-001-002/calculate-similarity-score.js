"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
var readline = require("readline");
// Function to calculate the similarity score
function calculateSimilarityScore(filePath) {
    // Create an interface to read the file line by line
    var fileStream = fs.createReadStream(filePath);
    var rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity, // Handle both '\n' and '\r\n' line breaks
    });
    // Arrays to store the left and right lists of numbers
    var leftList = [];
    var rightList = [];
    // Read each line from the file
    rl.on('line', function (line) {
        var parts = line.trim().split(/\s+/); // Split by any whitespace
        if (parts.length === 2) {
            var left = parseInt(parts[0]);
            var right = parseInt(parts[1]);
            leftList.push(left);
            rightList.push(right);
        }
    });
    // After reading all lines, process the lists
    rl.on('close', function () {
        // Create a map to count occurrences of each number in the right list
        var rightListCounts = new Map();
        for (var _i = 0, rightList_1 = rightList; _i < rightList_1.length; _i++) {
            var num = rightList_1[_i];
            rightListCounts.set(num, (rightListCounts.get(num) || 0) + 1);
        }
        // Calculate the similarity score
        var similarityScore = 0;
        for (var _a = 0, leftList_1 = leftList; _a < leftList_1.length; _a++) {
            var left = leftList_1[_a];
            var count = rightListCounts.get(left) || 0;
            similarityScore += left * count;
        }
        // Output the similarity score
        console.log("Total Similarity Score: ".concat(similarityScore));
    });
}
// Define the file path for input data (adjust the path as needed)
var filePath = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-001\\input.txt';
// Call the function to calculate the similarity score
calculateSimilarityScore(filePath);
