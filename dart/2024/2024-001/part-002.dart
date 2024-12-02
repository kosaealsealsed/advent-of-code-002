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

  // Convert the right list into a frequency map
  final rightListCounts = <int, int>{};
  for (var number in rightList) {
    rightListCounts[number] = (rightListCounts[number] ?? 0) + 1;
  }

  // Calculate the similarity score
  final similarityScore = leftList
      .map((left) => left * (rightListCounts[left] ?? 0))
      .reduce((a, b) => a + b);

  print('Similarity Score: $similarityScore');
}
