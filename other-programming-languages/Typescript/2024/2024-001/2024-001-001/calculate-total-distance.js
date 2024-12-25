"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
var readline = require("readline");
// Function to calculate total distance
function calculateTotalDistance(filePath) {
    // Read the file and create an interface for reading line-by-line
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
        // Sort both lists
        leftList.sort(function (a, b) { return a - b; });
        rightList.sort(function (a, b) { return a - b; });
        // Calculate the total distance
        var totalDistance = 0;
        for (var i = 0; i < leftList.length; i++) {
            totalDistance += Math.abs(leftList[i] - rightList[i]);
        }
        // Output the total distance
        console.log("Total Distance: ".concat(totalDistance));
    });
}
// Define the file path for input data (adjust the path as needed)
var filePath = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-001\\input.txt';
// Call the function to calculate the total distance
calculateTotalDistance(filePath);
