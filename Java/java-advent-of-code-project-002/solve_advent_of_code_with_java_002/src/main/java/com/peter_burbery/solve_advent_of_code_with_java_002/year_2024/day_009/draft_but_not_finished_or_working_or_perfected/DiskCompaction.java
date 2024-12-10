package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_009;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Disk compaction utility for parsing disk maps, compacting blocks and files, and calculating checksums.
 */
public class DiskCompaction {

    /**
     * Parses the disk map into a list of blocks.
     *
     * @param diskMap String representing the disk map.
     * @return List of blocks parsed from the disk map.
     */
    public static List<Object> parseDiskMap(String diskMap) {
        List<Integer> lengths = Arrays.stream(diskMap.split(""))
                .map(Integer::parseInt)
                .toList();
        List<Object> disk = new ArrayList<>();
        int fileId = 0;

        for (int i = 0; i < lengths.size(); i++) {
            int length = lengths.get(i);
            if (i % 2 == 0) {
                if (length > 0) {
                    disk.addAll(Collections.nCopies(length, fileId));
                }
                fileId++;
            } else {
                if (length > 0) {
                    disk.addAll(Collections.nCopies(length, '.'));
                }
            }
        }

        return disk;
    }

    /**
     * Simulates moving individual blocks to compact the disk.
     *
     * @param disk List representing the disk.
     * @return Compact disk as a list.
     */
    public static List<Object> compactBlocks(List<Object> disk) {
        List<Object> diskList = new ArrayList<>(disk);

        while (true) {
            int gapIndex = diskList.indexOf('.'); // Use List's indexOf method
            if (gapIndex == -1) break;

            int lastIndex = diskList.size() - 1;
            while (lastIndex > gapIndex && diskList.get(lastIndex).equals('.')) {
                lastIndex--;
            }
            if (lastIndex == gapIndex) break; // All remaining blocks are gaps

            Object rightBlock = diskList.remove(lastIndex); // Remove the block from the end
            diskList.set(gapIndex, rightBlock); // Place it in the gap
        }

        return diskList;
    }


    /**
     * Simulates moving entire files to compact the disk.
     *
     * @param disk  List representing the disk.
     * @param files Map containing file information.
     * @return Compact disk as a list.
     */
    public static List<Object> compactFiles(List<Object> disk, Map<Integer, FileInfo> files) {
        files.keySet().stream().sorted(Comparator.reverseOrder()).forEach(fileId -> {
            FileInfo fileInfo = files.get(fileId);
            int fileLength = fileInfo.length;
            int fileStart = fileInfo.start;

            Integer targetStart = findSegmentForFile(disk, fileLength, fileStart);
            if (targetStart != null) {
                for (int pos = fileStart; pos < fileStart + fileLength; pos++) {
                    disk.set(pos, '.');
                }
                for (int pos = targetStart; pos < targetStart + fileLength; pos++) {
                    disk.set(pos, fileId);
                }
            }
        });

        return disk;
    }

    /**
     * Finds a segment for the file on the disk.
     *
     * @param disk      List representing the disk.
     * @param fileLength Length of the file.
     * @param maxIndex  Maximum index to search.
     * @return Starting index of the segment or null if not found.
     */
    private static Integer findSegmentForFile(List<Object> disk, int fileLength, int maxIndex) {
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

    /**
     * Calculates the checksum of the disk.
     *
     * @param disk List representing the disk.
     * @return Checksum value.
     */
    public static int calculateChecksum(List<Object> disk) {
        int checksum = 0;
        for (int i = 0; i < disk.size(); i++) {
            Object block = disk.get(i);
            if (!block.equals('.')) {
                checksum += i * (int) block;
            }
        }
        return checksum;
    }

    /**
     * Extracts information about each file on the disk.
     *
     * @param disk List representing the disk.
     * @return Map of file information.
     */
    public static Map<Integer, FileInfo> getFileInfo(List<Object> disk) {
        Map<Integer, FileInfo> files = new HashMap<>();

        for (int i = 0; i < disk.size(); i++) {
            Object block = disk.get(i);
            if (!block.equals('.')) {
                int fileId = (int) block;
                final int currentIndex = i; // Create a final variable for `i`
                files.computeIfAbsent(fileId, id -> new FileInfo(currentIndex, 0));
                files.get(fileId).length++;
            }
        }

        return files;
    }


    /**
     * Main function for reading disk maps, parsing, compacting, and calculating checksums.
     *
     * @param args Command-line arguments.
     * @throws IOException If reading the file fails.
     */
    public static void main(String[] args) throws IOException {
        // Read the disk map from input.txt
        String diskMap = Files.readString(Path.of("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-009\\input.txt")).trim();

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
        Map<Integer, FileInfo> files = getFileInfo(disk);

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

    /**
     * Data class to store file information.
     */
    static class FileInfo {
        int start;
        int length;

        FileInfo(int start, int length) {
            this.start = start;
            this.length = length;
        }
    }
}

