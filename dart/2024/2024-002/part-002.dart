import 'dart:io';

bool isSafe(List<int> report) {
  List<int> differences = [
    for (int i = 0; i < report.length - 1; i++) report[i + 1] - report[i]
  ];
  bool allIncreasing = differences.every((diff) => diff >= 1 && diff <= 3);
  bool allDecreasing = differences.every((diff) => diff <= -1 && diff >= -3);
  return allIncreasing || allDecreasing;
}

bool isSafeWithDampener(List<int> report) {
  if (isSafe(report)) {
    return true;
  }

  for (int i = 0; i < report.length; i++) {
    List<int> modifiedReport = [
      ...report.sublist(0, i),
      ...report.sublist(i + 1)
    ];
    if (isSafe(modifiedReport)) {
      return true;
    }
  }
  return false;
}

void main() async {
  // File path
  String inputPath = r'\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-002\input.txt';

  // Read the input file
  List<String> lines = await File(inputPath).readAsLines();

  // Parse the input into a list of lists of integers
  List<List<int>> reports = lines.map((line) {
    return line.split(' ').map(int.parse).toList();
  }).toList();

  // Count safe reports
  int safeCount = reports.where(isSafe).length;
  print('Safe count: $safeCount');

  // Count safe reports with the problem dampener
  int safeWithDampenerCount = reports.where(isSafeWithDampener).length;
  print('Safe with dampener count: $safeWithDampenerCount');
}
