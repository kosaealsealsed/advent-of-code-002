const fs = require('fs');

// Function to calculate the similarity score
function calculateSimilarityScore(filePath) {
    // Read the file and split the content into lines
    const data = fs.readFileSync(filePath, 'utf-8').split('\n');

    // Initialize arrays for left and right lists
    let leftList = [];
    let rightList = [];

    // Process each line from the file
    data.forEach(line => {
        if (line.trim()) {
            const [left, right] = line.split(/\s+/).map(Number); // Split and convert to numbers
            leftList.push(left);
            rightList.push(right);
        }
    });

    // Create a dictionary to count occurrences of each number in the right list
    const rightListCounts = rightList.reduce((counts, num) => {
        counts[num] = (counts[num] || 0) + 1;
        return counts;
    }, {});

    // Calculate the similarity score
    const similarityScore = leftList.reduce((total, left) => {
        return total + (left * (rightListCounts[left] || 0));
    }, 0);

    // Output the similarity score
    console.log('Total Similarity Score:', similarityScore);
}

// File path for input data
const filePath = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\node-js\\2024\\2024_001\\input.txt';

// Call the function to calculate the similarity score
calculateSimilarityScore(filePath);
