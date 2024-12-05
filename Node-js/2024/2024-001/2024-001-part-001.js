const fs = require('fs');

// File path
const filePath = "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\node-js\\2024\\2024_001\\input.txt";

// Read the file
fs.readFile(filePath, 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        return;
    }

    // Split the data into lines
    const lines = data.split('\n');

    // Initialize left and right lists
    const leftList = [];
    const rightList = [];

    // Process each line
    lines.forEach(line => {
        const parts = line.trim().split(/\s+/); // Split by whitespace
        if (parts.length === 2) {
            leftList.push(parseInt(parts[0], 10));
            rightList.push(parseInt(parts[1], 10));
        }
    });

    // Sort both lists
    leftList.sort((a, b) => a - b);
    rightList.sort((a, b) => a - b);

    // Calculate the total distance
    let totalDistance = 0;
    for (let i = 0; i < leftList.length; i++) {
        totalDistance += Math.abs(leftList[i] - rightList[i]);
    }

    // Output the total distance
    console.log("Total Distance:", totalDistance);
});
