package com.peter_burbery.solve_advent_of_code_with_java_002.year_2024.day_009;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class DiskCompaction {

	public static List<Object> parseDiskMap(String diskMap) {
		List<Integer> lengths = new ArrayList<>();
		for (char c : diskMap.toCharArray()) {
			lengths.add(Character.getNumericValue(c));
		}

		List<Object> disk = new ArrayList<>();
		int fileId = 0;
		for (int i = 0; i < lengths.size(); i++) {
			int length = lengths.get(i);
			if (i % 2 == 0) {
				if (length > 0) {
					for (int j = 0; j < length; j++) {
						disk.add(fileId);
					}
				}
				fileId++;
			} else {
				if (length > 0) {
					for (int j = 0; j < length; j++) {
						disk.add('.');
					}
				}
			}
		}
		return disk;
	}

	public static List<Object> compactBlocks(List<Object> disk) {
		int gapIndex = disk.indexOf('.');
		if (gapIndex == -1) {
			return disk; // No gaps to compact
		}

		for (int i = disk.size() - 1; i > gapIndex; i--) {
			if (!disk.get(i).equals('.')) {
				disk.set(gapIndex, disk.get(i));
				disk.set(i, '.');
				gapIndex++;
			}
		}

		return disk;
	}

	public static List<Object> compactFiles(List<Object> disk, Map<Integer, Map<String, Integer>> files) {
		for (int fId : files.keySet()) {
			Map<String, Integer> fInfo = files.get(fId);
			int fLength = fInfo.get("length");
			int fStart = fInfo.get("start");
			Integer targetStart = findSegmentForFile(disk, fLength, fStart);
			if (targetStart != null) {
				for (int pos = fStart; pos < fStart + fLength; pos++) {
					disk.set(pos, '.');
				}
				for (int pos = targetStart; pos < targetStart + fLength; pos++) {
					disk.set(pos, fId);
				}
			}
		}
		return disk;
	}

	public static Integer findSegmentForFile(List<Object> disk, int fileLength, int maxIndex) {
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

	public static long calculateChecksum(List<Object> disk) {
		long checksum = 0;
		for (int i = 0; i < disk.size(); i++) {
			Object block = disk.get(i);
			if (!block.equals('.')) {
				checksum += i * (int) block;
			}
		}
		return checksum;
	}

	public static Map<Integer, Map<String, Integer>> getFileInfo(List<Object> disk) {
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
		String diskMap = Files.readString(Paths
				.get("\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-009\\input.txt"))
				.strip();

		// Parse the disk map
		long startTotal = System.nanoTime();
		long startPart1 = System.nanoTime();
		List<Object> disk = parseDiskMap(diskMap);
		long endPart1 = System.nanoTime();

		System.out.printf("Part 1 disk parsing done in %.9f s%n", (endPart1 - startPart1) / 1e9);

		// Compact blocks (Part 1)
		long startCompactBlocks = System.nanoTime();
		List<Object> compactedDisk = compactBlocks(disk);
		long part1Checksum = calculateChecksum(compactedDisk);
		long endCompactBlocks = System.nanoTime();

		System.out.printf("Part 1 answer is %d, found in %.9f s%n", part1Checksum,
				(endCompactBlocks - startCompactBlocks) / 1e9);

		// Gather file information
		Map<Integer, Map<String, Integer>> files = getFileInfo(disk);

		// Compact files (Part 2)
		long startPart2 = System.nanoTime();
		List<Object> compactedDiskFiles = compactFiles(new ArrayList<>(disk), files);
		long part2Checksum = calculateChecksum(compactedDiskFiles);
		long endPart2 = System.nanoTime();

		System.out.printf("Part 2 answer is %d, found in %.9f s%n", part2Checksum, (endPart2 - startPart2) / 1e9);

		// Total time
		long endTotal = System.nanoTime();
		System.out.printf("Total time was %.9f s%n", (endTotal - startTotal) / 1e9);
	}
}
