import * as fs from "fs";

// Function to calculate the sum of all mul(X, Y) operations
function sumMulOperations(corruptedMemory: string): number {
    const pattern = /mul\((\d{1,3}),(\d{1,3})\)/g;
    let total = 0;
    let match: RegExpExecArray | null;

    while ((match = pattern.exec(corruptedMemory)) !== null) {
        const num1 = parseInt(match[1], 10);
        const num2 = parseInt(match[2], 10);
        total += num1 * num2;
    }

    return total;
}

// Function to calculate the sum of enabled mul(X, Y) operations
function sumEnabledMulOperations(corruptedMemory: string): number {
    const pattern = /(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))/g;
    let totalSum = 0;
    let mulEnabled = true;
    let match: RegExpExecArray | null;

    while ((match = pattern.exec(corruptedMemory)) !== null) {
        const fullMatch = match[1];

        if (fullMatch === "do()") {
            mulEnabled = true;
        } else if (fullMatch === "don't()") {
            mulEnabled = false;
        } else if (mulEnabled && match[2] !== undefined && match[3] !== undefined) {
            const num1 = parseInt(match[2], 10);
            const num2 = parseInt(match[3], 10);
            totalSum += num1 * num2;
        }
    }

    return totalSum;
}

// Main logic
const filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-003\\input.txt";
const corruptedMemory = fs.readFileSync(filePath, "utf-8");

// Calculate the results
const totalSumAllMulOperations = sumMulOperations(corruptedMemory);
const totalSumEnabledMulOperations = sumEnabledMulOperations(corruptedMemory);

// Output the results
console.log(`The sum of all mul operations is: ${totalSumAllMulOperations}`);
console.log(`The sum of enabled mul operations is: ${totalSumEnabledMulOperations}`);
