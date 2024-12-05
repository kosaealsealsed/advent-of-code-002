// Reading the file content

var content = read('/uploads/input.txt'); // Using SpiderMonkey's `read` instead of `readFileSync`.

// Splitting content into rules and updates
var sections = content.trim().split("\n\n");
var rulesSection = sections[0];
var updatesSection = sections[1];

var rules = rulesSection.split("\n").map(function(line) {
    var parts = line.split("|").map(Number);
    return [parts[0], parts[1]];
});

var updates = updatesSection.split("\n").map(function(line) {
    return line.split(",").map(Number);
});

// Helper function to check if an update follows the rules
function isUpdateOrdered(update, rules) {
    var indexMap = new Map();
    update.forEach(function(page, index) {
        indexMap.set(page, index);
    });

    for (var i = 0; i < rules.length; i++) {
        var rule = rules[i];
        var x = rule[0];
        var y = rule[1];
        if (indexMap.has(x) && indexMap.has(y) && indexMap.get(x) > indexMap.get(y)) {
            return false;
        }
    }
    return true;
}

// Identify correctly ordered updates and their middle page numbers
var correctUpdates = [];
var middlePages = [];

updates.forEach(function(update) {
    if (isUpdateOrdered(update, rules)) {
        correctUpdates.push(update);
        middlePages.push(update[Math.floor(update.length / 2)]);
    }
});

// Calculate the sum of middle pages
var sumMiddlePages = middlePages.reduce(function(acc, val) {
    return acc + val;
}, 0);
print("Sum of middle pages for correctly ordered updates:", sumMiddlePages);

// Helper function to sort an update according to the rules
function topologicalSort(update, rules) {
    var graph = new Map();
    var inDegree = new Map();
    var nodes = new Set(update);

    rules.forEach(function(rule) {
        var x = rule[0];
        var y = rule[1];
        if (nodes.has(x) && nodes.has(y)) {
            if (!graph.has(x)) graph.set(x, []);
            graph.get(x).push(y);

            inDegree.set(y, (inDegree.get(y) || 0) + 1);
            if (!inDegree.has(x)) inDegree.set(x, 0);
        }
    });

    var queue = Array.from(nodes).filter(function(node) {
        return (inDegree.get(node) || 0) === 0;
    });

    var sortedUpdate = [];

    while (queue.length > 0) {
        var current = queue.shift();
        sortedUpdate.push(current);

        (graph.get(current) || []).forEach(function(neighbor) {
            inDegree.set(neighbor, inDegree.get(neighbor) - 1);
            if (inDegree.get(neighbor) === 0) queue.push(neighbor);
        });
    }

    return sortedUpdate;
}

// Correct the incorrectly ordered updates and find their middle page numbers
var incorrectUpdates = [];
var incorrectMiddlePages = [];

updates.forEach(function(update) {
    if (!isUpdateOrdered(update, rules)) {
        var correctedUpdate = topologicalSort(update, rules);
        incorrectUpdates.push(correctedUpdate);
        incorrectMiddlePages.push(correctedUpdate[Math.floor(correctedUpdate.length / 2)]);
    }
});

// Calculate the sum of middle pages for corrected updates
var sumIncorrectMiddlePages = incorrectMiddlePages.reduce(function(acc, val) {
    return acc + val;
}, 0);
print("Sum of middle pages for corrected updates:", sumIncorrectMiddlePages);
