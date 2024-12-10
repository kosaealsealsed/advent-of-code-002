package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_009;

import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

public class DiskCompactor {

    public static List<Object> parseDiskMap(String diskMap) {
        // Parses the disk map into a list of blocks
        List<Integer> lengths = Arrays.stream(diskMap.split(""))
                                      .map(Integer::parseInt)
                                      .collect(Collectors.toList());
        List<Object> disk = new ArrayList<>();
        int fileId = 0;
        for (int i = 0; i < lengths.size(); i++) {
            int length = lengths.get(i);
            if (length > 0) {
                if (i % 2 == 0) {
                    // Add file blocks
                    for (int j = 0; j < length; j++) {
                        disk.add(fileId);
                    }
                    fileId++;
                } else {
                    // Add empty blocks
                    for (int j = 0; j < length; j++) {
                        disk.add('.');
                    }
                }
            }
        }
        return disk;
    }

    public static List<Object> compactBlocks(List<Object> disk) {
        // Simulates moving individual blocks to compact the disk
        Deque<Object> diskDeque = new ArrayDeque<>(disk);
        try {
            while (true) {
                int gapIndex = diskDeque.stream().toList().indexOf('.');
                Object rightBlock = diskDeque.pollLast();
                while (rightBlock.equals('.')) {
                    rightBlock = diskDeque.pollLast();
                }
                List<Object> temp = new ArrayList<>(diskDeque);
                temp.set(gapIndex, rightBlock);
                diskDeque = new ArrayDeque<>(temp);
            }
        } catch (IndexOutOfBoundsException | NoSuchElementException e) {
            // No more gaps to fill
        }
        return new ArrayList<>(diskDeque);
    }

    public static List<Object> compactFiles(List<Object> disk, Map<Integer, Map<String, Integer>> files) {
        // Simulates moving entire files to compact the disk
        for (Integer fileId : files.keySet().stream().sorted(Comparator.reverseOrder()).toList()) {
            Map<String, Integer> fileInfo = files.get(fileId);
            int fileLength = fileInfo.get("length");
            int fileStart = fileInfo.get("start");

            Integer targetStart = findSegmentForFile(disk, fileLength, disk.size());
            if (targetStart != null) {
                // Clear original file
                for (int i = fileStart; i < fileStart + fileLength; i++) {
                    disk.set(i, '.');
                }
                // Move file to the target segment
                for (int i = targetStart; i < targetStart + fileLength; i++) {
                    disk.set(i, fileId);
                }
            }
        }
        return disk;
    }

    private static Integer findSegmentForFile(List<Object> disk, int fileLength, int maxIndex) {
        // Finds a segment of empty blocks to fit a file
        int count = 0;
        Integer segmentStart = null;
        for (int i = 0; i < maxIndex; i++) {
            if (disk.get(i).equals('.')) {
                if (segmentStart == null) {
                    segmentStart = i;
                }
                count++;
                if (count == fileLength) {
                    return segmentStart;
                }
            } else {
                count = 0;
                segmentStart = null;
            }
        }
        return null;
    }

    public static int calculateChecksum(List<Object> disk) {
        // Calculates the checksum of the disk
        int checksum = 0;
        for (int i = 0; i < disk.size(); i++) {
            Object block = disk.get(i);
            if (!block.equals('.')) {
                checksum += i * (int) block;
            }
        }
        return checksum;
    }

    public static Map<Integer, Map<String, Integer>> getFileInfo(List<Object> disk) {
        // Extracts information about each file on the disk
        Map<Integer, Map<String, Integer>> files = new HashMap<>();
        for (int i = 0; i < disk.size(); i++) {
            Object block = disk.get(i);
            if (!block.equals('.')) {
                int fileId = (int) block;
                files.putIfAbsent(fileId, new HashMap<>(Map.of("start", i, "length", 0)));
                files.get(fileId).put("length", files.get(fileId).get("length") + 1);
            }
        }
        return files;
    }

    public static void main(String[] args) throws IOException {
        // Read the disk map from input.txt
        BufferedReader reader = new BufferedReader(new FileReader("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-009\\input.txt"));
        String diskMap = reader.readLine().strip();
        reader.close();

        long startTotal = System.nanoTime();

        // Parse the disk map
        long startPart1 = System.nanoTime();
        List<Object> disk = parseDiskMap(diskMap);
        long endPart1 = System.nanoTime();
        System.out.printf("Part 1 disk parsing done in %.9f s%n", (endPart1 - startPart1) / 1e9);

        // Compact blocks (Part 1)
        long startCompactBlocks = System.nanoTime();
        List<Object> compactedDisk = compactBlocks(disk);
        int part1Checksum = calculateChecksum(compactedDisk);
        long endCompactBlocks = System.nanoTime();
        System.out.printf("Part 1 answer is %d, found in %.9f s%n", part1Checksum, (endCompactBlocks - startCompactBlocks) / 1e9);

        // Gather file information
        Map<Integer, Map<String, Integer>> files = getFileInfo(disk);

        // Compact files (Part 2)
        long startPart2 = System.nanoTime();
        List<Object> compactedDiskFiles = compactFiles(new ArrayList<>(disk), files);
        int part2Checksum = calculateChecksum(compactedDiskFiles);
        long endPart2 = System.nanoTime();
        System.out.printf("Part 2 answer is %d, found in %.9f s%n", part2Checksum, (endPart2 - startPart2) / 1e9);

        // Total time
        long endTotal = System.nanoTime();
        System.out.printf("Total time was %.9f s%n", (endTotal - startTotal) / 1e9);
    }
}
