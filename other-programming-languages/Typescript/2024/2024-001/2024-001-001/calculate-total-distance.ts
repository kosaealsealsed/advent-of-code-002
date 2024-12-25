import * as fs from 'fs';
import * as readline from 'readline';

// Function to calculate total distance
function calculateTotalDistance(filePath: string): void {
  // Read the file and create an interface for reading line-by-line
  const fileStream = fs.createReadStream(filePath);
  const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity,  // Handle both '\n' and '\r\n' line breaks
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
    // Sort both lists
    leftList.sort((a, b) => a - b);
    rightList.sort((a, b) => a - b);

    // Calculate the total distance
    let totalDistance = 0;
    for (let i = 0; i < leftList.length; i++) {
      totalDistance += Math.abs(leftList[i] - rightList[i]);
    }

    // Output the total distance
    console.log(`Total Distance: ${totalDistance}`);
  });
}

// Define the file path for input data (adjust the path as needed)
const filePath = '\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-001\\input.txt';

// Call the function to calculate the total distance
calculateTotalDistance(filePath);
