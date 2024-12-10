//package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_009;
//
//import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.util.*;
//import java.util.stream.Collectors;
//
//public class DiskFragmenter {
//
//    public static List<Object> parseDiskMap(String diskMap) {
//        var lengths = diskMap.chars()
//                .mapToObj(c -> Character.getNumericValue(c))
//                .toList();
//
//        var disk = new ArrayList<Object>();
//        var fileId = 0;
//
//        for (var i = 0; i < lengths.size(); i++) {
//            var length = lengths.get(i);
//            if (i % 2 == 0) {
//                disk.addAll(Collections.nCopies(length, fileId));
//                fileId++;
//            } else {
//                disk.addAll(Collections.nCopies(length, '.'));
//            }
//        }
//        return disk;
//    }
//
//    public static List<Object> compactBlocks(List<Object> disk) {
//        var deque = new ArrayDeque<>(disk);
//        var list = new ArrayList<>(deque); // Convert to List for index operations
//
//        while (list.contains('.')) {
//            var gapIndex = list.indexOf('.');
//            var rightBlock = deque.pollLast(); // Pop from the end
//            while (rightBlock.equals('.')) {
//                rightBlock = deque.pollLast(); // Keep removing until non-'.'
//            }
//            list.set(gapIndex, rightBlock);
//        }
//        return list;
//    }
//
//    public static Map<Integer, FileInfo> getFileInfo(List<Object> disk) {
//        var files = new HashMap<Integer, FileInfo>();
//
//        for (var i = 0; i < disk.size(); i++) {
//            var block = disk.get(i);
//            if (block instanceof Integer fileId) {
//                // Use a final local variable for the starting index
//                final var startIndex = i;
//                files.computeIfAbsent(fileId, id -> new FileInfo(startIndex, 0));
//                files.get(fileId).length++;
//            }
//        }
//        return files;
//    }
//
//    public static List<Object> compactFiles(List<Object> disk, Map<Integer, FileInfo> files) {
//        // Sort keys in reverse order without lambdas
//        var sortedKeys = new ArrayList<>(files.keySet());
//        sortedKeys.sort(Comparator.reverseOrder());
//
//        for (var fId : sortedKeys) {
//            var file = files.get(fId);
//            var fileLength = file.length;
//
//            // Find leftmost span of free space
//            var segmentStart = findFreeSegment(disk, fileLength, file.start);
//
//            if (segmentStart != -1) {
//                for (var pos = file.start; pos < file.start + fileLength; pos++) {
//                    disk.set(pos, '.');
//                }
//                for (var pos = segmentStart; pos < segmentStart + fileLength; pos++) {
//                    disk.set(pos, fId);
//                }
//            }
//        }
//        return disk;
//    }
//
//    private static int findFreeSegment(List<Object> disk, int fileLength, int maxIndex) {
//        var count = 0;
//        var segmentStart = -1;
//
//        for (var i = 0; i < maxIndex; i++) {
//            if (disk.get(i).equals('.')) {
//                if (segmentStart == -1) segmentStart = i;
//                count++;
//                if (count == fileLength) return segmentStart;
//            } else {
//                count = 0;
//                segmentStart = -1;
//            }
//        }
//        return -1;
//    }
//
//    public static long calculateChecksum(List<Object> disk) {
//        var checksum = 0L;
//        for (var i = 0; i < disk.size(); i++) {
//            if (disk.get(i) instanceof Integer fileId) {
//                checksum += i * fileId;
//            }
//        }
//        return checksum;
//    }
//
//    public static void main(String[] args) throws IOException {
//        var startTotal = System.nanoTime();
//
//        // Read input from file
//        var diskMap = Files.readString(Path.of("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-009\\input.txt")).strip();
//
//        // Parse the disk map
//        var startPart1 = System.nanoTime();
//        var disk = parseDiskMap(diskMap);
//        var endPart1 = System.nanoTime();
//
//        System.out.printf("Part 1 disk parsing done in %.9f s%n", (endPart1 - startPart1) / 1e9);
//
//        // Compact blocks (Part 1)
//        var startCompactBlocks = System.nanoTime();
//        var compactedDisk = compactBlocks(new ArrayList<>(disk));
//        var part1Checksum = calculateChecksum(compactedDisk);
//        var endCompactBlocks = System.nanoTime();
//
//        System.out.printf("Part 1 answer is %d, found in %.9f s%n", part1Checksum, (endCompactBlocks - startCompactBlocks) / 1e9);
//
//        // Gather file information
//        var files = getFileInfo(disk);
//
//        // Compact files (Part 2)
//        var startPart2 = System.nanoTime();
//        var compactedDiskFiles = compactFiles(new ArrayList<>(disk), files);
//        var part2Checksum = calculateChecksum(compactedDiskFiles);
//        var endPart2 = System.nanoTime();
//
//        System.out.printf("Part 2 answer is %d, found in %.9f s%n", part2Checksum, (endPart2 - startPart2) / 1e9);
//
//        // Total time
//        var endTotal = System.nanoTime();
//        System.out.printf("Total time was %.9f s%n", (endTotal - startTotal) / 1e9);
//    }
//
//    private static class FileInfo {
//        int start;
//        int length;
//
//        FileInfo(int start, int length) {
//            this.start = start;
//            this.length = length;
//        }
//    }
//}
package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_009;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Disk compaction utility that parses, compacts, and calculates checksums for a
 * disk map.
 */
public class DiskCompactorBroken {

	/**
	 * Parses the disk map into a list of blocks.
	 * 
	 * @param diskMap A string representing the disk map.
	 * @return A list of blocks.
	 */
	public static List<Object> parseDiskMap(String diskMap) {
		var lengths = Arrays.stream(diskMap.split("")).map(Integer::parseInt).collect(Collectors.toList());
		var disk = new ArrayList<Object>();
		var fileId = 0;

		for (var i = 0; i < lengths.size(); i++) {
			var length = lengths.get(i);
			if (i % 2 == 0) {
				if (length > 0) {
					for (var j = 0; j < length; j++) {
						disk.add(fileId);
					}
				}
				fileId++;
			} else {
				if (length > 0) {
					for (var j = 0; j < length; j++) {
						disk.add(".");
					}
				}
			}
		}
		return disk;
	}

	/**
	 * Simulates moving individual blocks to compact the disk.
	 * 
	 * @param disk The disk as a list of blocks.
	 * @return The compacted disk.
	 */
	public static List<Object> compactBlocks(List<Object> disk) {
		var diskDeque = new ArrayDeque<>(disk);

		while (true) {
			var gapIndex = -1;
			try {
				gapIndex = new ArrayList<>(diskDeque).indexOf(".");
			} catch (NoSuchElementException e) {
				break;
			}

			if (gapIndex == -1) {
				break;
			}

			var rightBlock = diskDeque.pollLast();
			while (rightBlock != null && rightBlock.equals(".")) {
				rightBlock = diskDeque.pollLast();
			}

			if (gapIndex >= 0 && rightBlock != null) {
				var diskList = new ArrayList<>(diskDeque);
				diskList.set(gapIndex, rightBlock);
				diskDeque.clear();
				diskDeque.addAll(diskList);
			}
		}
		return new ArrayList<>(diskDeque);
	}

	/**
	 * Simulates moving entire files to compact the disk.
	 * 
	 * @param disk  The disk as a list of blocks.
	 * @param files A map containing file information.
	 * @return The compacted disk.
	 */
	public static List<Object> compactFiles(List<Object> disk, Map<Object, Map<String, Integer>> files) {
    files.keySet().stream()
            .sorted(Comparator.comparing(key -> (Comparable<Object>) key).reversed()) // Sort in reverse order
            .forEach(fId -> {
                var fInfo = files.get(fId);
                if (fInfo == null) {
                    throw new IllegalArgumentException("File ID not found in files map: " + fId);
                }

                var fLength = fInfo.get("length");
                var fStart = fInfo.get("start");

                if (fLength == null || fStart == null) {
                    throw new IllegalArgumentException("Missing file info for file ID: " + fId);
                }

                var targetStart = findSegmentForFile(disk, fLength, fStart);

                if (targetStart != null) {
                    for (var pos = fStart; pos < fStart + fLength; pos++) {
                        disk.set(pos, ".");
                    }
                    for (var pos = targetStart; pos < targetStart + fLength; pos++) {
                        disk.set(pos, fId);
                    }
                }
            });
    return disk;
}

	private static Integer findSegmentForFile(List<Object> disk, int fileLength, int maxIndex) {
		var count = 0;
		Integer segmentStart = null;

		for (var i = 0; i < maxIndex; i++) {
			if (disk.get(i).equals(".")) {
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
	 * @param disk The disk as a list of blocks.
	 * @return The checksum.
	 */
	public static long calculateChecksum(List<Object> disk) {
		var checksum = 0L;
		for (var i = 0; i < disk.size(); i++) {
			if (!disk.get(i).equals(".")) {
				checksum += i * ((Integer) disk.get(i));
			}
		}
		return checksum;
	}

	/**
	 * Extracts information about each file on the disk.
	 * 
	 * @param disk The disk as a list of blocks.
	 * @return A map of file information.
	 */
	public static Map<Object, Map<String, Integer>> getFileInfo(List<Object> disk) {
		var files = new HashMap<Object, Map<String, Integer>>();

		for (var i = 0; i < disk.size(); i++) {
			var block = disk.get(i);
			if (!block.equals(".")) {
				files.putIfAbsent(block, new HashMap<>(Map.of("start", i, "length", 0)));
				files.get(block).merge("length", 1, Integer::sum);
			}
		}
		return files;
	}

	public static void main(String[] args) throws IOException {
		// Read the disk map from input.txt
		var diskMap = Files.readString(Path
				.of("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-009\\input.txt"))
				.strip();

		// Parse the disk map
		var startTotal = System.nanoTime();
		var startPart1 = System.nanoTime();
		var disk = parseDiskMap(diskMap);
		var endPart1 = System.nanoTime();
		System.out.printf("Part 1 disk parsing done in %.9f s%n", (endPart1 - startPart1) / 1e9);

		// Compact blocks (Part 1)
		var startCompactBlocks = System.nanoTime();
		var compactedDisk = compactBlocks(disk);
		var part1Checksum = calculateChecksum(compactedDisk);
		var endCompactBlocks = System.nanoTime();
		System.out.printf("Part 1 answer is %d, found in %.9f s%n", part1Checksum,
				(endCompactBlocks - startCompactBlocks) / 1e9);

		// Gather file information
		var files = getFileInfo(disk);

		// Compact files (Part 2)
		var startPart2 = System.nanoTime();
		var compactedDiskFiles = compactFiles(new ArrayList<>(disk), files);
		var part2Checksum = calculateChecksum(compactedDiskFiles);
		var endPart2 = System.nanoTime();
		System.out.printf("Part 2 answer is %d, found in %.9f s%n", part2Checksum, (endPart2 - startPart2) / 1e9);

		// Total time
		var endTotal = System.nanoTime();
		System.out.printf("Total time was %.9f s%n", (endTotal - startTotal) / 1e9);
	}
}
