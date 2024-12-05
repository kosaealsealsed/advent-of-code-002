const fs = require('fs');
const path = require('path');

// Reading the file content
const filePath = path.join(__dirname, 'input.txt');
const content = fs.readFileSync(filePath, 'utf8');

// Splitting content into rules and updates
const [rulesSection, updatesSection] = content.trim().split("\n\n");

const rules = rulesSection.split("\n").map(line => {
    const [x, y] = line.split("|").map(Number);
    return [x, y];
});

const updates = updatesSection.split("\n").map(line => 
    line.split(",").map(Number)
);

// Helper function to check if an update follows the rules
function isUpdateOrdered(update, rules) {
    const indexMap = new Map();
    update.forEach((page, index) => {
        indexMap.set(page, index);
    });

    for (const [x, y] of rules) {
        if (indexMap.has(x) && indexMap.has(y) && indexMap.get(x) > indexMap.get(y)) {
            return false;
        }
    }
    return true;
}

// Identify correctly ordered updates and their middle page numbers
const correctUpdates = [];
const middlePages = [];

updates.forEach(update => {
    if (isUpdateOrdered(update, rules)) {
        correctUpdates.push(update);
        middlePages.push(update[Math.floor(update.length / 2)]);
    }
});

// Calculate the sum of middle pages
const sumMiddlePages = middlePages.reduce((acc, val) => acc + val, 0);
console.log("Sum of middle pages for correctly ordered updates:", sumMiddlePages);

// Helper function to sort an update according to the rules
function topologicalSort(update, rules) {
    const graph = new Map();
    const inDegree = new Map();
    const nodes = new Set(update);

    rules.forEach(([x, y]) => {
        if (nodes.has(x) && nodes.has(y)) {
            if (!graph.has(x)) graph.set(x, []);
            graph.get(x).push(y);

            inDegree.set(y, (inDegree.get(y) || 0) + 1);
            if (!inDegree.has(x)) inDegree.set(x, 0);
        }
    });

    const queue = Array.from(nodes).filter(node => (inDegree.get(node) || 0) === 0);
    const sortedUpdate = [];

    while (queue.length > 0) {
        const current = queue.shift();
        sortedUpdate.push(current);

        (graph.get(current) || []).forEach(neighbor => {
            inDegree.set(neighbor, inDegree.get(neighbor) - 1);
            if (inDegree.get(neighbor) === 0) queue.push(neighbor);
        });
    }

    return sortedUpdate;
}

// Correct the incorrectly ordered updates and find their middle page numbers
const incorrectUpdates = [];
const incorrectMiddlePages = [];

updates.forEach(update => {
    if (!isUpdateOrdered(update, rules)) {
        const correctedUpdate = topologicalSort(update, rules);
        incorrectUpdates.push(correctedUpdate);
        incorrectMiddlePages.push(correctedUpdate[Math.floor(correctedUpdate.length / 2)]);
    }
});

// Calculate the sum of middle pages for corrected updates
const sumIncorrectMiddlePages = incorrectMiddlePages.reduce((acc, val) => acc + val, 0);
console.log("Sum of middle pages for corrected updates:", sumIncorrectMiddlePages);
