"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
// Function to calculate the sum of all mul(X, Y) operations
function sumMulOperations(corruptedMemory) {
    var pattern = /mul\((\d{1,3}),(\d{1,3})\)/g;
    var total = 0;
    var match;
    while ((match = pattern.exec(corruptedMemory)) !== null) {
        var num1 = parseInt(match[1], 10);
        var num2 = parseInt(match[2], 10);
        total += num1 * num2;
    }
    return total;
}
// Function to calculate the sum of enabled mul(X, Y) operations
function sumEnabledMulOperations(corruptedMemory) {
    var pattern = /(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))/g;
    var totalSum = 0;
    var mulEnabled = true;
    var match;
    while ((match = pattern.exec(corruptedMemory)) !== null) {
        var fullMatch = match[1];
        if (fullMatch === "do()") {
            mulEnabled = true;
        }
        else if (fullMatch === "don't()") {
            mulEnabled = false;
        }
        else if (mulEnabled && match[2] !== undefined && match[3] !== undefined) {
            var num1 = parseInt(match[2], 10);
            var num2 = parseInt(match[3], 10);
            totalSum += num1 * num2;
        }
    }
    return totalSum;
}
// Main logic
var filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-003\\input.txt";
var corruptedMemory = fs.readFileSync(filePath, "utf-8");
// Calculate the results
var totalSumAllMulOperations = sumMulOperations(corruptedMemory);
var totalSumEnabledMulOperations = sumEnabledMulOperations(corruptedMemory);
// Output the results
console.log("The sum of all mul operations is: ".concat(totalSumAllMulOperations));
console.log("The sum of enabled mul operations is: ".concat(totalSumEnabledMulOperations));
