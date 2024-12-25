const fs = require('fs');
const path = require('path');

// Helper class to hold test cases
class TestCase {
    constructor(target, numbers) {
        this.target = target;
        this.numbers = numbers;
    }
}

// Evaluate expression left to right with + and *
function evaluateLeftToRight(numbers, ops) {
    let result = numbers[0];
    for (let i = 0; i < ops.length; i++) {
        const op = ops[i];
        const nextNumber = numbers[i + 1];
        switch (op) {
            case '+':
                result += nextNumber;
                break;
            case '*':
                result *= nextNumber;
                break;
            default:
                throw new Error(`Unsupported operator: ${op}`);
        }
    }
    return result;
}

// Evaluate expression left to right with +, *, and ||
function evaluateWithConcat(numbers, ops) {
    let result = numbers[0];
    for (let i = 0; i < ops.length; i++) {
        const op = ops[i];
        const nextNumber = numbers[i + 1];
        switch (op) {
            case '+':
                result += nextNumber;
                break;
            case '*':
                result *= nextNumber;
                break;
            case '||':
                result = parseInt(`${result}${nextNumber}`, 10);
                break;
            default:
                throw new Error(`Unsupported operator: ${op}`);
        }
    }
    return result;
}

// Parse input file into a list of test cases
function parseInput(filePath) {
    const testCases = [];
    const fileContent = fs.readFileSync(filePath, 'utf8');
    const lines = fileContent.split('\n');
    
    lines.forEach(line => {
        if (!line.includes(':')) return;

        const [targetPart, numbersPart] = line.split(':').map(part => part.trim());
        if (!targetPart || !numbersPart) return;

        const target = parseInt(targetPart, 10);
        const numbers = numbersPart.split(/\s+/).map(num => parseInt(num, 10));

        testCases.push(new TestCase(target, numbers));
    });

    return testCases;
}

// Generate all possible operator combinations
function generateOperatorCombinations(operators, length) {
    if (length === 0) return [[]];
    const subCombinations = generateOperatorCombinations(operators, length - 1);
    const combinations = [];
    subCombinations.forEach(sub => {
        operators.forEach(op => {
            combinations.push([...sub, op]);
        });
    });
    return combinations;
}

// Solve Part One using only + and * operators
function solvePartOne(testCases) {
    const operators = ['+', '*'];
    let validTestValuesSum = 0;

    testCases.forEach(testCase => {
        const opsLength = testCase.numbers.length - 1;
        const allOps = generateOperatorCombinations(operators, opsLength);

        const possible = allOps.some(ops => evaluateLeftToRight(testCase.numbers, ops) === testCase.target);
        if (possible) {
            validTestValuesSum += testCase.target;
        }
    });

    return validTestValuesSum;
}

// Solve Part Two using +, *, and || operators
function solvePartTwo(testCases, partOneTime) {
    const operators = ['+', '*', '||'];
    let validTestValuesSum = 0;

    console.log("\nProgress    Interval(s)      Cumulative1(s)    Cumulative2(s)");
    console.log("-------------------------------------------------------------");

    const totalCases = testCases.length;
    const progressInterval = Math.max(Math.floor(totalCases / 10), 1);

    let cumulative1 = 0.0; // Total elapsed time for Part 2 progress
    let cumulative2 = partOneTime; // Cumulative time including Part 1

    const startTime = process.hrtime.bigint();

    testCases.forEach((testCase, index) => {
        const opsLength = testCase.numbers.length - 1;
        const allOps = generateOperatorCombinations(operators, opsLength);

        const possible = allOps.some(ops => evaluateWithConcat(testCase.numbers, ops) === testCase.target);
        if (possible) {
            validTestValuesSum += testCase.target;
        }

        if ((index + 1) % progressInterval === 0 || index + 1 === totalCases) {
            const intervalElapsed = Number(process.hrtime.bigint() - startTime) / 1e9;
            cumulative1 += intervalElapsed;
            cumulative2 = partOneTime + cumulative1;

            console.log(
                `${index + 1}/${totalCases}      ${intervalElapsed.toFixed(9)}   ${cumulative1.toFixed(9)}   ${cumulative2.toFixed(9)}`
            );
        }
    });

    return { validTestValuesSum, cumulative2 };
}

// Main method
function main() {
    const filePath = path.join('\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-007\\input.txt');

    // Parse the input
    const testCases = parseInput(filePath);

    // Solve Part One
    console.log("Starting Part 1...");
    const partOneStartTime = process.hrtime.bigint();
    const partOneResult = solvePartOne(testCases);
    const partOneTime = Number(process.hrtime.bigint() - partOneStartTime) / 1e9;
    console.log(`Part 1 finished in ${partOneTime.toFixed(9)} seconds`);
    console.log(`Part 1 Total Calibration Result: ${partOneResult}`);

    // Solve Part Two
    console.log("Starting Part 2...");
    const { validTestValuesSum: partTwoResult, cumulative2: cumulativeTime } = solvePartTwo(testCases, partOneTime);
    const partTwoTime = cumulativeTime - partOneTime;
    console.log(`\nPart 2 finished in ${partTwoTime.toFixed(9)} seconds, cumulative time ${cumulativeTime.toFixed(9)} seconds`);
    console.log(`Part 2 Total Calibration Result: ${partTwoResult}`);
}

// Run the program
main();
