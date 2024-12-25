import * as fs from 'fs';

// Reading the file content
const filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-005\\input.txt";
const content = fs.readFileSync(filePath, 'utf8');

// Splitting content into rules and updates
const [rulesSection, updatesSection] = content.trim().split("\n\n");

const rules: [number, number][] = rulesSection.split("\n").map(line => {
    const [x, y] = line.split("|").map(Number);
    return [x, y];
});

const updates: number[][] = updatesSection.split("\n").map(line =>
    line.split(",").map(Number)
);

// Helper function to check if an update follows the rules
function isUpdateOrdered(update: number[], rules: [number, number][]): boolean {
    const indexMap: Record<number, number> = {};
    update.forEach((page, index) => {
        indexMap[page] = index;
    });

    for (const [x, y] of rules) {
        if (x in indexMap && y in indexMap && indexMap[x] > indexMap[y]) {
            return false;
        }
    }
    return true;
}

// Identify correctly ordered updates and their middle page numbers
const correctUpdates: number[][] = [];
const middlePages: number[] = [];

for (const update of updates) {
    if (isUpdateOrdered(update, rules)) {
        correctUpdates.push(update);
        middlePages.push(update[Math.floor(update.length / 2)]);
    }
}

// Calculate the sum of middle pages
const sumMiddlePages = middlePages.reduce((acc, val) => acc + val, 0);
console.log("Sum of middle pages for correctly ordered updates:", sumMiddlePages);

// Helper function to sort an update according to the rules
function topologicalSort(update: number[], rules: [number, number][]): number[] {
    const graph: Record<number, number[]> = {};
    const inDegree: Record<number, number> = {};
    const nodes = new Set(update);

    rules.forEach(([x, y]) => {
        if (nodes.has(x) && nodes.has(y)) {
            if (!graph[x]) graph[x] = [];
            graph[x].push(y);
            inDegree[y] = (inDegree[y] || 0) + 1;
            if (!(x in inDegree)) inDegree[x] = 0;
        }
    });

    const queue: number[] = Array.from(nodes).filter(node => (inDegree[node] || 0) === 0);
    const sortedUpdate: number[] = [];

    while (queue.length > 0) {
        const current = queue.shift()!;
        sortedUpdate.push(current);

        (graph[current] || []).forEach(neighbor => {
            inDegree[neighbor]--;
            if (inDegree[neighbor] === 0) queue.push(neighbor);
        });
    }

    return sortedUpdate;
}

// Correct the incorrectly ordered updates and find their middle page numbers
const incorrectUpdates: number[][] = [];
const incorrectMiddlePages: number[] = [];

for (const update of updates) {
    if (!isUpdateOrdered(update, rules)) {
        const correctedUpdate = topologicalSort(update, rules);
        incorrectUpdates.push(correctedUpdate);
        incorrectMiddlePages.push(correctedUpdate[Math.floor(correctedUpdate.length / 2)]);
    }
}

// Calculate the sum of middle pages for corrected updates
const sumIncorrectMiddlePages = incorrectMiddlePages.reduce((acc, val) => acc + val, 0);
console.log("Sum of middle pages for corrected updates:", sumIncorrectMiddlePages);
