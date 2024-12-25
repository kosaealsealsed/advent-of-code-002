import * as fs from 'fs';
import * as readline from 'readline';

// Function to calculate the similarity score
function calculateSimilarityScore(filePath: string): void {
  // Create an interface to read the file line by line
  const fileStream = fs.createReadStream(filePath);
  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity, // Handle both '\n' and '\r\n' line breaks
  });

  // Arrays to store the left and right lists of numbers
  const leftList: number[] = [];
  const rightList: number[] = [];

  // Read each line from the file
  rl.on('line', (line: string) => {
    const parts = line.trim().split(/\s+/);  // Split by any whitespace
    if (parts.length === 2) {
      const left = parseInt(parts[0]);
      const right = parseInt(parts[1]);
      leftList.push(left);
      rightList.push(right);
    }
  });

  // After reading all lines, process the lists
  rl.on('close', () => {
    // Create a map to count occurrences of each number in the right list
    const rightListCounts = new Map<number, number>();
    for (const num of rightList) {
      rightListCounts.set(num, (rightListCounts.get(num) || 0) + 1);
    }

    // Calculate the similarity score
    let similarityScore = 0;
    for (const left of leftList) {
      const count = rightListCounts.get(left) || 0;
      similarityScore += left * count;
    }

    // Output the similarity score
    console.log(`Total Similarity Score: ${similarityScore}`);
  });
}

// Define the file path for input data (adjust the path as needed)
const filePath = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-001\\input.txt';

// Call the function to calculate the similarity score
calculateSimilarityScore(filePath);
