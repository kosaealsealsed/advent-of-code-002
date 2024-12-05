using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace UpdateProcessorApp
{
    class Program
    {
        static void Main(string[] args)
        {
            string filePath = "input.txt";
            string content;

            // Reading the file content
            try
            {
                content = File.ReadAllText(filePath).Trim();
            }
            catch (IOException e)
            {
                Console.Error.WriteLine($"Error reading the file: {e.Message}");
                return;
            }

            // Splitting content into rules and updates
            string[] sections = content.Split(new string[] { "\n\n" }, StringSplitOptions.None);
            if (sections.Length != 2)
            {
                Console.Error.WriteLine("Invalid input format. Expected two sections separated by two newlines.");
                return;
            }

            string rulesSection = sections[0];
            string updatesSection = sections[1];

            // Parsing rules
            List<(int, int)> rules = new List<(int, int)>();
            foreach (var ruleLine in rulesSection.Split(new[] { "\r\n", "\n" }, StringSplitOptions.None))
            {
                if (string.IsNullOrWhiteSpace(ruleLine))
                    continue;

                string[] parts = ruleLine.Split('|');
                if (parts.Length != 2)
                {
                    Console.Error.WriteLine($"Invalid rule format: {ruleLine}");
                    continue;
                }

                if (int.TryParse(parts[0].Trim(), out int x) && int.TryParse(parts[1].Trim(), out int y))
                {
                    rules.Add((x, y));
                }
                else
                {
                    Console.Error.WriteLine($"Invalid numbers in rule: {ruleLine}");
                }
            }

            // Parsing updates
            List<List<int>> updates = new List<List<int>>();
            foreach (var updateLine in updatesSection.Split(new[] { "\r\n", "\n" }, StringSplitOptions.None))
            {
                if (string.IsNullOrWhiteSpace(updateLine))
                    continue;

                string[] parts = updateLine.Split(',');
                List<int> update = new List<int>();
                bool valid = true;

                foreach (var part in parts)
                {
                    if (int.TryParse(part.Trim(), out int page))
                    {
                        update.Add(page);
                    }
                    else
                    {
                        Console.Error.WriteLine($"Invalid number in update: {updateLine}");
                        valid = false;
                        break;
                    }
                }

                if (valid)
                {
                    updates.Add(update);
                }
            }

            // Identify correctly ordered updates and their middle page numbers
            List<List<int>> correctUpdates = new List<List<int>>();
            List<int> middlePages = new List<int>();

            foreach (var update in updates)
            {
                if (IsUpdateOrdered(update, rules))
                {
                    correctUpdates.Add(update);
                    middlePages.Add(GetMiddlePage(update));
                }
            }

            // Calculate the sum of middle pages for correct updates
            long sumMiddlePages = middlePages.Sum(page => (long)page);
            Console.WriteLine($"Sum of middle pages for correctly ordered updates: {sumMiddlePages}");

            // Identify incorrectly ordered updates, correct them, and collect their middle pages
            List<List<int>> incorrectUpdates = new List<List<int>>();
            List<int> incorrectMiddlePages = new List<int>();

            foreach (var update in updates)
            {
                if (!IsUpdateOrdered(update, rules))
                {
                    List<int> correctedUpdate = TopologicalSortUpdate(update, rules);
                    if (correctedUpdate.Count == 0)
                    {
                        Console.Error.WriteLine($"Cycle detected or unable to sort update: {string.Join(", ", update)}");
                        continue;
                    }
                    incorrectUpdates.Add(correctedUpdate);
                    incorrectMiddlePages.Add(GetMiddlePage(correctedUpdate));
                }
            }

            // Calculate the sum of middle pages for corrected updates
            long sumIncorrectMiddlePages = incorrectMiddlePages.Sum(page => (long)page);
            Console.WriteLine($"Sum of middle pages for corrected updates: {sumIncorrectMiddlePages}");
        }

        /// <summary>
        /// Checks if the given update follows all the specified rules.
        /// </summary>
        /// <param name="update">The list of page numbers in the update.</param>
        /// <param name="rules">The list of rules as tuples where (x, y) means x should come before y.</param>
        /// <returns>True if the update is correctly ordered according to the rules, false otherwise.</returns>
        private static bool IsUpdateOrdered(List<int> update, List<(int, int)> rules)
        {
            Dictionary<int, int> indexMap = new Dictionary<int, int>();
            for (int i = 0; i < update.Count; i++)
            {
                indexMap[update[i]] = i;
            }

            foreach (var rule in rules)
            {
                int x = rule.Item1;
                int y = rule.Item2;

                if (indexMap.ContainsKey(x) && indexMap.ContainsKey(y))
                {
                    if (indexMap[x] > indexMap[y])
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        /// <summary>
        /// Performs a topological sort on the given update based on the specified rules.
        /// </summary>
        /// <param name="update">The list of page numbers in the update.</param>
        /// <param name="rules">The list of rules as tuples where (x, y) means x should come before y.</param>
        /// <returns>A sorted list of page numbers that adheres to the rules. Returns an empty list if a cycle is detected.</returns>
        private static List<int> TopologicalSortUpdate(List<int> update, List<(int, int)> rules)
        {
            // Build the graph and in-degree count based on the rules
            Dictionary<int, List<int>> graph = new Dictionary<int, List<int>>();
            Dictionary<int, int> inDegree = new Dictionary<int, int>();
            HashSet<int> nodes = new HashSet<int>(update);

            foreach (var node in nodes)
            {
                graph[node] = new List<int>();
                inDegree[node] = 0;
            }

            foreach (var rule in rules)
            {
                int x = rule.Item1;
                int y = rule.Item2;

                if (nodes.Contains(x) && nodes.Contains(y))
                {
                    graph[x].Add(y);
                    inDegree[y]++;
                }
            }

            // Initialize the queue with nodes having in-degree 0
            Queue<int> queue = new Queue<int>();
            foreach (var node in nodes)
            {
                if (inDegree[node] == 0)
                {
                    queue.Enqueue(node);
                }
            }

            List<int> sortedUpdate = new List<int>();

            while (queue.Count > 0)
            {
                int current = queue.Dequeue();
                sortedUpdate.Add(current);

                foreach (var neighbor in graph[current])
                {
                    inDegree[neighbor]--;
                    if (inDegree[neighbor] == 0)
                    {
                        queue.Enqueue(neighbor);
                    }
                }
            }

            // Check if topological sort was possible (i.e., no cycles)
            if (sortedUpdate.Count != nodes.Count)
            {
                // Cycle detected or unable to sort
                return new List<int>();
            }

            return sortedUpdate;
        }

        /// <summary>
        /// Retrieves the middle page number from the update list.
        /// </summary>
        /// <param name="update">The list of page numbers.</param>
        /// <returns>The middle page number.</returns>
        private static int GetMiddlePage(List<int> update)
        {
            return update[update.Count / 2];
        }
    }
}
