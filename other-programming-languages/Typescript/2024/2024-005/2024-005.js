"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var fs = require("fs");
// Reading the file content
var filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-005\\input.txt";
var content = fs.readFileSync(filePath, 'utf8');
// Splitting content into rules and updates
var _a = content.trim().split("\n\n"), rulesSection = _a[0], updatesSection = _a[1];
var rules = rulesSection.split("\n").map(function (line) {
    var _a = line.split("|").map(Number), x = _a[0], y = _a[1];
    return [x, y];
});
var updates = updatesSection.split("\n").map(function (line) {
    return line.split(",").map(Number);
});
// Helper function to check if an update follows the rules
function isUpdateOrdered(update, rules) {
    var indexMap = {};
    update.forEach(function (page, index) {
        indexMap[page] = index;
    });
    for (var _i = 0, rules_1 = rules; _i < rules_1.length; _i++) {
        var _a = rules_1[_i], x = _a[0], y = _a[1];
        if (x in indexMap && y in indexMap && indexMap[x] > indexMap[y]) {
            return false;
        }
    }
    return true;
}
// Identify correctly ordered updates and their middle page numbers
var correctUpdates = [];
var middlePages = [];
for (var _i = 0, updates_1 = updates; _i < updates_1.length; _i++) {
    var update = updates_1[_i];
    if (isUpdateOrdered(update, rules)) {
        correctUpdates.push(update);
        middlePages.push(update[Math.floor(update.length / 2)]);
    }
}
// Calculate the sum of middle pages
var sumMiddlePages = middlePages.reduce(function (acc, val) { return acc + val; }, 0);
console.log("Sum of middle pages for correctly ordered updates:", sumMiddlePages);
// Helper function to sort an update according to the rules
function topologicalSort(update, rules) {
    var graph = {};
    var inDegree = {};
    var nodes = new Set(update);
    rules.forEach(function (_a) {
        var x = _a[0], y = _a[1];
        if (nodes.has(x) && nodes.has(y)) {
            if (!graph[x])
                graph[x] = [];
            graph[x].push(y);
            inDegree[y] = (inDegree[y] || 0) + 1;
            if (!(x in inDegree))
                inDegree[x] = 0;
        }
    });
    var queue = Array.from(nodes).filter(function (node) { return (inDegree[node] || 0) === 0; });
    var sortedUpdate = [];
    while (queue.length > 0) {
        var current = queue.shift();
        sortedUpdate.push(current);
        (graph[current] || []).forEach(function (neighbor) {
            inDegree[neighbor]--;
            if (inDegree[neighbor] === 0)
                queue.push(neighbor);
        });
    }
    return sortedUpdate;
}
// Correct the incorrectly ordered updates and find their middle page numbers
var incorrectUpdates = [];
var incorrectMiddlePages = [];
for (var _b = 0, updates_2 = updates; _b < updates_2.length; _b++) {
    var update = updates_2[_b];
    if (!isUpdateOrdered(update, rules)) {
        var correctedUpdate = topologicalSort(update, rules);
        incorrectUpdates.push(correctedUpdate);
        incorrectMiddlePages.push(correctedUpdate[Math.floor(correctedUpdate.length / 2)]);
    }
}
// Calculate the sum of middle pages for corrected updates
var sumIncorrectMiddlePages = incorrectMiddlePages.reduce(function (acc, val) { return acc + val; }, 0);
console.log("Sum of middle pages for corrected updates:", sumIncorrectMiddlePages);
