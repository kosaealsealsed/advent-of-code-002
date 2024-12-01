using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ConsoleApp1
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Similarity Score Calculation Program");

            // File path
            string filePath = @"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-001\input.txt";

            try
            {
                // Read all lines from the file
                var lines = File.ReadAllLines(filePath);

                // Initialize lists for the two columns
                var leftList = new List<int>();
                var rightList = new List<int>();

                // Parse each line and populate the lists
                foreach (var line in lines)
                {
                    var columns = line.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                    if (columns.Length >= 2)
                    {
                        leftList.Add(int.Parse(columns[0]));
                        rightList.Add(int.Parse(columns[1]));
                    }
                }

                // Count occurrences of each number in the right list
                var rightListCounts = rightList.GroupBy(x => x).ToDictionary(g => g.Key, g => g.Count());

                // Calculate the similarity score
                int similarityScore = leftList
                    .Select(left => left * (rightListCounts.ContainsKey(left) ? rightListCounts[left] : 0))
                    .Sum();

                // Output the similarity score
                Console.WriteLine($"Similarity Score: {similarityScore}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
    }
}
