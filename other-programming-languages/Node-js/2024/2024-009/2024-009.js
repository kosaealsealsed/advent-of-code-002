const fs = require('fs');

class FileInfo {
  constructor(start, length) {
    this.start = start;
    this.length = length;
  }
}

function parseDiskMap(diskMap) {
  const lengths = [...diskMap].map(Number);
  const disk = [];
  let fileId = 0;

  for (let i = 0; i < lengths.length; i++) {
    const length = lengths[i];
    if (i % 2 === 0) {
      // Add file blocks
      for (let j = 0; j < length; j++) {
        disk.push(fileId);
      }
      fileId++;
    } else {
      // Add free space
      for (let j = 0; j < length; j++) {
        disk.push('.');
      }
    }
  }

  return disk;
}

function compactBlocks(disk) {
  const deque = [...disk]; // Use array as deque for simplicity
  const list = [...deque]; // Copy for indexed operations

  while (list.includes('.')) {
    const gapIndex = list.indexOf('.');
    let rightBlock = deque.pop();

    while (rightBlock === '.') {
      rightBlock = deque.pop();
    }

    list[gapIndex] = rightBlock;
  }

  return list;
}

function getFileInfo(disk) {
  const files = new Map();

  disk.forEach((block, i) => {
    if (typeof block === 'number') {
      if (!files.has(block)) {
        files.set(block, new FileInfo(i, 0));
      }
      files.get(block).length++;
    }
  });

  return files;
}

function findFreeSegment(disk, fileLength, maxIndex) {
  let count = 0;
  let segmentStart = -1;

  for (let i = 0; i < maxIndex; i++) {
    if (disk[i] === '.') {
      if (segmentStart === -1) {
        segmentStart = i;
      }
      count++;
      if (count === fileLength) return segmentStart;
    } else {
      count = 0;
      segmentStart = -1;
    }
  }

  return -1;
}

function compactFiles(disk, files) {
  const sortedKeys = [...files.keys()].sort((a, b) => b - a);

  sortedKeys.forEach((fId) => {
    const file = files.get(fId);
    const fileLength = file.length;

    const segmentStart = findFreeSegment(disk, fileLength, file.start);

    if (segmentStart !== -1) {
      for (let pos = file.start; pos < file.start + fileLength; pos++) {
        disk[pos] = '.';
      }
      for (let pos = segmentStart; pos < segmentStart + fileLength; pos++) {
        disk[pos] = fId;
      }
    }
  });

  return disk;
}

function calculateChecksum(disk) {
  return disk.reduce((checksum, block, i) => {
    if (typeof block === 'number') {
      return checksum + i * block;
    }
    return checksum;
  }, 0);
}

function main() {
  const startTotal = process.hrtime.bigint();

  // Read input from file
  const diskMap = fs
    .readFileSync("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-009\\input.txt", 'utf-8')
    .trim();

  // Parse the disk map
  const startPart1 = process.hrtime.bigint();
  const disk = parseDiskMap(diskMap);
  const endPart1 = process.hrtime.bigint();

  console.log(
    `Part 1 disk parsing done in ${Number(endPart1 - startPart1) / 1e9} s`
  );

  // Compact blocks (Part 1)
  const startCompactBlocks = process.hrtime.bigint();
  const compactedDisk = compactBlocks([...disk]);
  const part1Checksum = calculateChecksum(compactedDisk);
  const endCompactBlocks = process.hrtime.bigint();

  console.log(
    `Part 1 answer is ${part1Checksum}, found in ${
      Number(endCompactBlocks - startCompactBlocks) / 1e9
    } s`
  );

  // Gather file information
  const files = getFileInfo(disk);

  // Compact files (Part 2)
  const startPart2 = process.hrtime.bigint();
  const compactedDiskFiles = compactFiles([...disk], files);
  const part2Checksum = calculateChecksum(compactedDiskFiles);
  const endPart2 = process.hrtime.bigint();

  console.log(
    `Part 2 answer is ${part2Checksum}, found in ${
      Number(endPart2 - startPart2) / 1e9
    } s`
  );

  // Total time
  const endTotal = process.hrtime.bigint();
  console.log(`Total time was ${Number(endTotal - startTotal) / 1e9} s`);
}

main();
