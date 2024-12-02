using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class SafeReports
{
    // Method to check if a report is safe
    static bool IsSafe(List<int> report)
    {
        var differences = report.Zip(report.Skip(1), (a, b) => b - a).ToList();
        
        bool allIncreasing = differences.All(diff => diff >= 1 && diff <= 3);
        bool allDecreasing = differences.All(diff => diff <= -1 && diff >= -3);

        return allIncreasing || allDecreasing;
    }

    // Method to check if a report is safe with the Problem Dampener
    static bool IsSafeWithDampener(List<int> report)
    {
        if (IsSafe(report))
        {
            return true;
        }

        for (int i = 0; i < report.Count; i++)
        {
            var modifiedReport = report.Take(i).Concat(report.Skip(i + 1)).ToList();
            if (IsSafe(modifiedReport))
            {
                return true;
            }
        }

        return false;
    }

    static void Main(string[] args)
    {
        string inputPath = "input.txt";

        // Read input lines from the file
        var lines = File.ReadAllLines(inputPath);
        var reports = lines.Select(line => line.Split(' ').Select(int.Parse).ToList()).ToList();

        // Count safe reports
        int safeCount = reports.Count(IsSafe);
        Console.WriteLine($"Safe reports: {safeCount}");

        // Count safe reports with the Problem Dampener
        int safeWithDampenerCount = reports.Count(IsSafeWithDampener);
        Console.WriteLine($"Safe reports with dampener: {safeWithDampenerCount}");
    }
}
