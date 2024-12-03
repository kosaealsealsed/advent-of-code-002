using System;
using System.IO;
using System.Text.RegularExpressions;

class Program
{
    // Function to sum all mul(X, Y) operations
    static int SumMulOperations(string corruptedMemory)
    {
        string pattern = @"mul\((\d{1,3}),(\d{1,3})\)";
        Regex regex = new Regex(pattern);
        MatchCollection matches = regex.Matches(corruptedMemory);
        int total = 0;

        foreach (Match match in matches)
        {
            int num1 = int.Parse(match.Groups[1].Value);
            int num2 = int.Parse(match.Groups[2].Value);
            total += num1 * num2;
        }

        return total;
    }

    // Function to sum only enabled mul(X, Y) operations
    static int SumEnabledMulOperations(string corruptedMemory)
    {
        string pattern = @"(do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\))";
        Regex regex = new Regex(pattern);
        MatchCollection matches = regex.Matches(corruptedMemory);
        int totalSum = 0;
        bool mulEnabled = true; // mul instructions are enabled at the start

        foreach (Match match in matches)
        {
            string fullMatch = match.Groups[1].Value;

            if (fullMatch == "do()")
            {
                mulEnabled = true;
            }
            else if (fullMatch == "don't()")
            {
                mulEnabled = false;
            }
            else if (match.Groups[2].Success && match.Groups[3].Success)
            {
                // This is a mul(X, Y) instruction
                if (mulEnabled)
                {
                    int num1 = int.Parse(match.Groups[2].Value);
                    int num2 = int.Parse(match.Groups[3].Value);
                    totalSum += num1 * num2;
                }
            }
        }

        return totalSum;
    }

    static void Main(string[] args)
    {
        // Read the corrupted memory from 'input.txt'
        string filePath = "input.txt";
        string corruptedMemory = File.ReadAllText(filePath);

        // Calculate the results
        int totalSumAllMulOperations = SumMulOperations(corruptedMemory);
        int totalSumEnabledMulOperations = SumEnabledMulOperations(corruptedMemory);

        // Output the results
        Console.WriteLine($"The sum of all mul operations is: {totalSumAllMulOperations}");
        Console.WriteLine($"The sum of enabled mul operations is: {totalSumEnabledMulOperations}");
    }
}
