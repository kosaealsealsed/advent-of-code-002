const fs = require('fs');
const path = require('path');

// Function to check if a report is safe
function isSafe(report) {
    const differences = report.slice(1).map((num, i) => num - report[i]);

    const allIncreasing = differences.every(diff => diff >= 1 && diff <= 3);
    const allDecreasing = differences.every(diff => diff <= -1 && diff >= -3);

    return allIncreasing || allDecreasing;
}

// Function to check if a report is safe with the Problem Dampener
function isSafeWithDampener(report) {
    if (isSafe(report)) {
        return true;
    }

    for (let i = 0; i < report.length; i++) {
        const modifiedReport = report.slice(0, i).concat(report.slice(i + 1));
        if (isSafe(modifiedReport)) {
            return true;
        }
    }

    return false;
}

// Main function to process the input and count safe reports
function main() {
    const inputPath = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-002\\input.txt';
    const filePath = path.resolve(inputPath);

    // Read input file
    const data = fs.readFileSync(filePath, 'utf8');
    const lines = data.trim().split('\n');

    // Parse the reports
    const reports = lines.map(line => line.split(/\s+/).map(Number));

    // Count safe reports
    const safeCount = reports.filter(isSafe).length;
    console.log(`Safe reports: ${safeCount}`);

    // Count safe reports with the Problem Dampener
    const safeWithDampenerCount = reports.filter(isSafeWithDampener).length;
    console.log(`Safe reports with dampener: ${safeWithDampenerCount}`);
}

// Run the main function
main();
