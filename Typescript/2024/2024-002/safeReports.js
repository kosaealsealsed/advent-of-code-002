"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
var path = require("path");
// Function to check if a report is safe
function isSafe(report) {
    var differences = report.slice(1).map(function (num, i) { return num - report[i]; });
    var allIncreasing = differences.every(function (diff) { return diff >= 1 && diff <= 3; });
    var allDecreasing = differences.every(function (diff) { return diff <= -1 && diff >= -3; });
    return allIncreasing || allDecreasing;
}
// Function to check if a report is safe with the Problem Dampener
function isSafeWithDampener(report) {
    if (isSafe(report)) {
        return true;
    }
    for (var i = 0; i < report.length; i++) {
        var modifiedReport = report.slice(0, i).concat(report.slice(i + 1));
        if (isSafe(modifiedReport)) {
            return true;
        }
    }
    return false;
}
// Main function to process the input and count safe reports
function main() {
    var inputPath = path.resolve("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-002\\input.txt");
    // Read the input file
    var data = fs.readFileSync(inputPath, "utf-8");
    var lines = data.trim().split("\n");
    // Convert input data into an array of reports
    var reports = lines.map(function (line) { return line.split(/\s+/).map(Number); });
    // Count the number of safe reports
    var safeCount = reports.filter(isSafe).length;
    console.log("Safe reports: ".concat(safeCount));
    // Count the number of safe reports with the Problem Dampener
    var safeWithDampenerCount = reports.filter(isSafeWithDampener).length;
    console.log("Safe reports with dampener: ".concat(safeWithDampenerCount));
}
// Run the main function
main();
