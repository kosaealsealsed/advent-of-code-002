import 'dart:io';

bool isSafe(List<int> report) {
  List<int> differences = List.generate(
    report.length - 1,
    (i) => report[i + 1] - report[i],
  );

  bool allIncreasing = differences.every((diff) => diff >= 1 && diff <= 3);
  bool allDecreasing = differences.every((diff) => diff <= -1 && diff >= -3);

  return allIncreasing || allDecreasing;
}

bool isSafeWithDampener(List<int> report) {
  if (isSafe(report)) {
    return true;
  }

  for (int i = 0; i < report.length; i++) {
    List<int> modifiedReport = List.from(report)..removeAt(i);
    if (isSafe(modifiedReport)) {
      return true;
    }
  }

  return false;
}

void main() async {
  const inputPath = '\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-002\input.txt';

  // Read the input file
  final lines = await File(inputPath).readAsLines();
  final reports = lines
      .map((line) => line.split(RegExp(r'\s+')).map(int.parse).toList())
      .toList();

  // Count safe reports
  final safeCount = reports.where(isSafe).length;
  print('Safe reports: $safeCount');

  // Count safe reports with the Problem Dampener
  final safeWithDampenerCount = reports.where(isSafeWithDampener).length;
  print('Safe reports with dampener: $safeWithDampenerCount');
}
