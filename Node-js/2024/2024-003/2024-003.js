const fs = require('fs');

function sumMulOperations(corruptedMemory) {
    // Define the regex pattern for valid mul instructions
    const pattern = /mul\((\d{1,3}),(\d{1,3})\)/g;
    let total = 0;
    let match;

    // Iterate over all matches in the corrupted memory
    while ((match = pattern.exec(corruptedMemory)) !== null) {
        const num1 = parseInt(match[1], 10);
        const num2 = parseInt(match[2], 10);
        total += num1 * num2;
    }

    return total;
}

function sumEnabledMulOperations(corruptedMemory) {
    // Define regex patterns for do(), don't(), and mul(X,Y)
    const pattern = /(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))/g;
    let totalSum = 0;
    let mulEnabled = true; // mul instructions are enabled at the start
    let match;

    // Iterate over all matches in the corrupted memory
    while ((match = pattern.exec(corruptedMemory)) !== null) {
        const fullMatch = match[1];

        if (fullMatch === 'do()') {
            mulEnabled = true;
        } else if (fullMatch === "don't()") {
            mulEnabled = false;
        } else if (match[2] && match[3]) {
            // This is a mul(X,Y) instruction
            if (mulEnabled) {
                const num1 = parseInt(match[2], 10);
                const num2 = parseInt(match[3], 10);
                totalSum += num1 * num2;
            }
        }
    }

    return totalSum;
}

// Read the corrupted memory from 'input.txt'
const filePath = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-003\\input.txt';
const corruptedMemory = fs.readFileSync(filePath, 'utf-8');

// Calculate the sums
const totalSumAllMulOperations = sumMulOperations(corruptedMemory);
const totalSumEnabledMulOperations = sumEnabledMulOperations(corruptedMemory);

// Output both results
console.log(`The sum of all mul operations is: ${totalSumAllMulOperations}`);
console.log(`The sum of enabled mul operations is: ${totalSumEnabledMulOperations}`);