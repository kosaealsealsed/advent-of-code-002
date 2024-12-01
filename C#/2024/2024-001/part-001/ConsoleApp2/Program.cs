using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ConsoleApp2
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Distance Calculation Program");

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

                // Sort both lists
                leftList.Sort();
                rightList.Sort();

                // Calculate the total distance between the two lists
                int totalDistance = leftList.Zip(rightList, (left, right) => Math.Abs(left - right)).Sum();

                // Output the total distance
                Console.WriteLine($"Total Distance: {totalDistance}");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error: {ex.Message}");
            }
        }
    }
}
