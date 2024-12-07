using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ConsoleApp3_2024_007_Bridge_repair
{
    class Program
    {
        // Define a TestCase class to hold target and numbers
        class TestCase
        {
            public long Target { get; }
            public List<long> Numbers { get; }

            public TestCase(long target, List<long> numbers)
            {
                Target = target;
                Numbers = numbers;
            }
        }

        // Evaluate expression left to right with + and *
        static long EvaluateLeftToRight(List<long> numbers, List<string> ops)
        {
            long result = numbers[0];
            for (int i = 0; i < ops.Count; i++)
            {
                string op = ops[i];
                long nextNumber = numbers[i + 1];
                switch (op)
                {
                    case "+":
                        result += nextNumber;
                        break;
                    case "*":
                        result *= nextNumber;
                        break;
                    default:
                        throw new ArgumentException($"Unsupported operator: {op}");
                }
            }
            return result;
        }

        // Evaluate expression left to right with +, *, and ||
        static long EvaluateWithConcat(List<long> numbers, List<string> ops)
        {
            long result = numbers[0];
            for (int i = 0; i < ops.Count; i++)
            {
                string op = ops[i];
                long nextNumber = numbers[i + 1];
                switch (op)
                {
                    case "+":
                        result += nextNumber;
                        break;
                    case "*":
                        result *= nextNumber;
                        break;
                    case "||":
                        result = long.Parse($"{result}{nextNumber}");
                        break;
                    default:
                        throw new ArgumentException($"Unsupported operator: {op}");
                }
            }
            return result;
        }

        // Parse input file into a list of TestCase
        static List<TestCase> ParseInput(string filePath)
        {
            var testCases = new List<TestCase>();
            foreach (var line in File.ReadLines(filePath))
            {
                if (!line.Contains(":")) continue;

                var parts = line.Split(":");
                if (parts.Length != 2) continue;

                if (!long.TryParse(parts[0].Trim(), out var target)) continue;

                var numbers = parts[1].Trim().Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries)
                               .Select(long.Parse).ToList();

                testCases.Add(new TestCase(target, numbers));
            }
            return testCases;
        }

        // Generate all possible operator combinations
        static List<List<string>> GenerateOperatorCombinations(List<string> operators, int length)
        {
            if (length == 0) return new List<List<string>> { new List<string>() };

            var subCombinations = GenerateOperatorCombinations(operators, length - 1);
            var combinations = new List<List<string>>();
            foreach (var sub in subCombinations)
            {
                foreach (var op in operators)
                {
                    var newCombination = new List<string>(sub) { op };
                    combinations.Add(newCombination);
                }
            }
            return combinations;
        }

        // Solve Part One using only + and * operators
        static long SolvePartOne(List<TestCase> testCases)
        {
            var operators = new List<string> { "+", "*" };
            long validTestValuesSum = 0;

            foreach (var testCase in testCases)
            {
                bool possible = false;
                int opsLength = testCase.Numbers.Count - 1;
                var allOps = GenerateOperatorCombinations(operators, opsLength);

                foreach (var ops in allOps)
                {
                    if (EvaluateLeftToRight(testCase.Numbers, ops) == testCase.Target)
                    {
                        possible = true;
                        break;
                    }
                }
                if (possible)
                {
                    validTestValuesSum += testCase.Target;
                }
            }
            return validTestValuesSum;
        }

        // Solve Part Two using +, *, and || operators with progress reporting
        static (long validTestValuesSum, double cumulativeTime) SolvePartTwo(List<TestCase> testCases, double partOneTime)
        {
            var operators = new List<string> { "+", "*", "||" };
            long validTestValuesSum = 0;
            int totalCases = testCases.Count;
            int progressInterval = Math.Max(1, totalCases / 10);

            double cumulative1 = 0.0;
            double cumulative2 = partOneTime;

            Console.WriteLine("\nProgress    Interval(s)      Cumulative1(s)    Cumulative2(s)");
            Console.WriteLine("-------------------------------------------------------------");

            var startTime = DateTime.UtcNow;

            for (int index = 0; index < totalCases; index++)
            {
                var testCase = testCases[index];
                bool possible = false;
                int opsLength = testCase.Numbers.Count - 1;
                var allOps = GenerateOperatorCombinations(operators, opsLength);

                foreach (var ops in allOps)
                {
                    if (EvaluateWithConcat(testCase.Numbers, ops) == testCase.Target)
                    {
                        possible = true;
                        break;
                    }
                }
                if (possible)
                {
                    validTestValuesSum += testCase.Target;
                }

                // Progress reporting
                if ((index + 1) % progressInterval == 0 || (index + 1) == totalCases)
                {
                    var intervalElapsed = (DateTime.UtcNow - startTime).TotalSeconds;
                    cumulative1 += intervalElapsed;
                    cumulative2 = partOneTime + cumulative1;

                    Console.WriteLine($"{index + 1}/{totalCases}      {intervalElapsed:F9}   {cumulative1:F9}   {cumulative2:F9}");
                    startTime = DateTime.UtcNow;
                }
            }
            return (validTestValuesSum, cumulative2);
        }

        static void Main(string[] args)
        {
            // Update filePath with the input file location
            var filePath = @"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-007\input.txt";

            // Parse the input
            var testCases = ParseInput(filePath);

            // Solve Part One
            Console.WriteLine("Starting Part 1...");
            var partOneStartTime = DateTime.UtcNow;
            var partOneResult = SolvePartOne(testCases);
            var partOneTime = (DateTime.UtcNow - partOneStartTime).TotalSeconds;
            Console.WriteLine($"Part 1 finished in {partOneTime:F9} seconds");
            Console.WriteLine($"Part 1 Total Calibration Result: {partOneResult}");

            // Solve Part Two
            Console.WriteLine("Starting Part 2...");
            var (partTwoResult, cumulativeTime) = SolvePartTwo(testCases, partOneTime);
            var partTwoTime = cumulativeTime - partOneTime;
            Console.WriteLine($"\nPart 2 finished in {partTwoTime:F9} seconds, cumulative time {cumulativeTime:F9} seconds");
            Console.WriteLine($"Part 2 Total Calibration Result: {partTwoResult}");
        }
    }
}
