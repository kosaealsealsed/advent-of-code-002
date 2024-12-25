import 'dart:io';

void main() async {
  // File path
  final filePath = r'\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-001\input.txt';

  // Read the file
  final file = File(filePath);
  final lines = await file.readAsLines();

  // Parse the lines into two lists of integers
  final leftList = <int>[];
  final rightList = <int>[];

  for (var line in lines) {
    final parts = line.split(RegExp(r'\s+')).map(int.parse).toList();
    leftList.add(parts[0]);
    rightList.add(parts[1]);
  }

  // Sort both lists
  leftList.sort();
  rightList.sort();

  // Calculate the total distance
  final totalDistance = leftList
      .asMap()
      .entries
      .map((entry) => (entry.value - rightList[entry.key]).abs())
      .reduce((a, b) => a + b);

  print('Total Distance: $totalDistance');
}
