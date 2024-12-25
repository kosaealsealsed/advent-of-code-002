import 'dart:io';
import 'dart:collection';

/// Reads and processes the input file, returning rules and updates as structured data.
Future<Map<String, dynamic>> readFile(String filePath) async {
  final fileContent = await File(filePath).readAsString();
  final sections = fileContent.trim().split("\n\n");

  // Parse rules
  final rules = sections[0]
      .split('\n')
      .map((line) {
        final parts = line.split('|').map(int.parse).toList();
        return {'x': parts[0], 'y': parts[1]};
      })
      .toList();

  // Parse updates
  final updates = sections[1]
      .split('\n')
      .map((line) => line.split(',').map(int.parse).toList())
      .toList();

  return {'rules': rules, 'updates': updates};
}

/// Checks if the given update follows the specified rules.
bool isUpdateOrdered(List<int> update, List<Map<String, int>> rules) {
  final indexMap = {for (var i = 0; i < update.length; i++) update[i]: i};
  for (var rule in rules) {
    final x = rule['x']!;
    final y = rule['y']!;
    if (indexMap.containsKey(x) &&
        indexMap.containsKey(y) &&
        indexMap[x]! > indexMap[y]!) {
      return false;
    }
  }
  return true;
}

/// Returns the middle page of a given update.
int findMiddlePage(List<int> update) {
  return update[update.length ~/ 2];
}

/// Identifies correctly ordered updates and calculates the sum of their middle pages.
int calculateMiddlePagesSum(List<List<int>> updates, List<Map<String, int>> rules) {
  final middlePages = updates
      .where((update) => isUpdateOrdered(update, rules))
      .map(findMiddlePage)
      .toList();
  return middlePages.fold(0, (sum, page) => sum + page);
}

/// Performs topological sort on an update based on the given rules and returns the sorted update.
List<int> topologicalSortUpdate(List<int> update, List<Map<String, int>> rules) {
  final graph = <int, List<int>>{};
  final inDegree = <int, int>{};
  final nodes = update.toSet();

  for (var rule in rules) {
    final x = rule['x']!;
    final y = rule['y']!;
    if (nodes.contains(x) && nodes.contains(y)) {
      graph.putIfAbsent(x, () => []).add(y);
      inDegree[y] = (inDegree[y] ?? 0) + 1;
      inDegree.putIfAbsent(x, () => 0);
    }
  }

  final queue = Queue<int>();
  nodes.forEach((node) {
    if ((inDegree[node] ?? 0) == 0) queue.add(node);
  });

  final sortedUpdate = <int>[];
  while (queue.isNotEmpty) {
    final current = queue.removeFirst();
    sortedUpdate.add(current);
    if (graph.containsKey(current)) {
      for (var neighbor in graph[current]!) {
        inDegree[neighbor] = inDegree[neighbor]! - 1;
        if (inDegree[neighbor] == 0) queue.add(neighbor);
      }
    }
  }

  return sortedUpdate;
}

/// Processes updates to identify incorrectly ordered updates, corrects them,
/// and calculates the sum of their middle pages.
int processUpdates(List<List<int>> updates, List<Map<String, int>> rules) {
  final correctedMiddlePages = <int>[];

  for (var update in updates) {
    if (!isUpdateOrdered(update, rules)) {
      final correctedUpdate = topologicalSortUpdate(update, rules);
      correctedMiddlePages.add(findMiddlePage(correctedUpdate));
    }
  }

  return correctedMiddlePages.fold(0, (sum, page) => sum + page);
}

void main() async {
  final filePath = 'input.txt';
  final inputData = await readFile(filePath);

  final rules = inputData['rules'] as List<Map<String, int>>;
  final updates = inputData['updates'] as List<List<int>>;

  // Calculate sum of middle pages for correctly ordered updates
  final sumCorrectMiddlePages = calculateMiddlePagesSum(updates, rules);
  print('Sum of middle pages for correctly ordered updates: $sumCorrectMiddlePages');

  // Calculate sum of middle pages for corrected updates
  final sumIncorrectMiddlePages = processUpdates(updates, rules);
  print('Sum of middle pages for corrected updates: $sumIncorrectMiddlePages');
}
