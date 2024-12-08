using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace _2024_008
{
    /// <summary>
    /// Main class for computing antinodes using two methods: pairwise and line-drawing.
    /// </summary>
    public static class AntinodeCalculator
    {
        /// <summary>
        /// Reads the grid map from a file.
        /// </summary>
        /// <param name="filename">The name of the file containing the grid map.</param>
        /// <returns>A list of strings representing the grid.</returns>
        public static List<string> ReadMap(string filename)
        {
            return File.ReadAllLines(filename).ToList();
        }

        /// <summary>
        /// Computes unique antinode positions using the pairwise method.
        /// </summary>
        /// <param name="grid">A list of strings representing the grid map.</param>
        /// <returns>A set of unique antinode positions.</returns>
        public static HashSet<(int, int)> ComputeAntinodesPairwise(List<string> grid)
        {
            var rows = grid.Count;
            var cols = rows > 0 ? grid[0].Length : 0;

            var antennasByFreq = new Dictionary<char, List<(int, int)>>();

            for (int r = 0; r < rows; r++)
            {
                for (int c = 0; c < cols; c++)
                {
                    var ch = grid[r][c];
                    if (ch != '.')
                    {
                        if (!antennasByFreq.ContainsKey(ch))
                            antennasByFreq[ch] = new List<(int, int)>();
                        antennasByFreq[ch].Add((r, c));
                    }
                }
            }

            var antinodes = new HashSet<(int, int)>();

            foreach (var coords in antennasByFreq.Values)
            {
                var n = coords.Count;
                if (n < 2) continue;

                for (int i = 0; i < n; i++)
                {
                    var (rA, cA) = coords[i];
                    for (int j = i + 1; j < n; j++)
                    {
                        var (rB, cB) = coords[j];

                        // Compute P1 = 2B - A
                        var p1_r = 2 * rB - rA;
                        var p1_c = 2 * cB - cA;
                        if (p1_r >= 0 && p1_r < rows && p1_c >= 0 && p1_c < cols)
                        {
                            antinodes.Add((p1_r, p1_c));
                        }

                        // Compute P2 = 2A - B
                        var p2_r = 2 * rA - rB;
                        var p2_c = 2 * cA - cB;
                        if (p2_r >= 0 && p2_r < rows && p2_c >= 0 && p2_c < cols)
                        {
                            antinodes.Add((p2_r, p2_c));
                        }
                    }
                }
            }

            return antinodes;
        }

        /// <summary>
        /// Computes unique antinode positions using the line-drawing method.
        /// </summary>
        /// <param name="grid">A list of strings representing the grid map.</param>
        /// <returns>A set of unique antinode positions.</returns>
        public static HashSet<(int, int)> ComputeAntinodesLines(List<string> grid)
        {
            var rows = grid.Count;
            var cols = rows > 0 ? grid[0].Length : 0;

            var antennasByFreq = new Dictionary<char, List<(int, int)>>();

            for (int r = 0; r < rows; r++)
            {
                for (int c = 0; c < cols; c++)
                {
                    var ch = grid[r][c];
                    if (ch != '.')
                    {
                        if (!antennasByFreq.ContainsKey(ch))
                            antennasByFreq[ch] = new List<(int, int)>();
                        antennasByFreq[ch].Add((r, c));
                    }
                }
            }

            var antinodes = new HashSet<(int, int)>();

            foreach (var coords in antennasByFreq.Values)
            {
                var n = coords.Count;
                if (n < 2) continue;

                for (int i = 0; i < n; i++)
                {
                    var (rA, cA) = coords[i];
                    for (int j = i + 1; j < n; j++)
                    {
                        var (rB, cB) = coords[j];
                        AddLinePoints(rA, cA, rB, cB, rows, cols, antinodes);
                    }
                }
            }

            return antinodes;
        }

        /// <summary>
        /// Adds all points along a line between two antennas to the antinodes set.
        /// </summary>
        private static void AddLinePoints(int rA, int cA, int rB, int cB, int rows, int cols, HashSet<(int, int)> antinodes)
        {
            int dr = rB - rA, dc = cB - cA;
            var g = Gcd(Math.Abs(dr), Math.Abs(dc));
            dr /= g;
            dc /= g;

            // Add points in the forward direction
            var rP = rA;
            var cP = cA;
            while (rP >= 0 && rP < rows && cP >= 0 && cP < cols)
            {
                antinodes.Add((rP, cP));
                rP += dr;
                cP += dc;
            }

            // Add points in the backward direction
            rP = rA - dr;
            cP = cA - dc;
            while (rP >= 0 && rP < rows && cP >= 0 && cP < cols)
            {
                antinodes.Add((rP, cP));
                rP -= dr;
                cP -= dc;
            }
        }

        /// <summary>
        /// Computes the greatest common divisor of two integers.
        /// </summary>
        public static int Gcd(int a, int b)
        {
            return b == 0 ? a : Gcd(b, a % b);
        }

        /// <summary>
        /// Formats the elapsed time in seconds with nanoseconds as a human-readable string.
        /// </summary>
        private static string FormatTime(long nanos)
        {
            return $"{nanos / 1e9:F9} s";
        }

        /// <summary>
        /// Main function to run the program.
        /// </summary>
        public static void Main()
        {
            var grid = ReadMap(@"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-008\input.txt");

            // Overall timing starts here
            var overallStart = DateTime.UtcNow;

            // Part 1: Compute using Pairwise method
            var startTime = DateTime.UtcNow;
            var pairwiseAntinodes = ComputeAntinodesPairwise(grid);
            var part1Time = DateTime.UtcNow - startTime;
            Console.WriteLine($"Part 001 finished in {FormatTime(part1Time.Ticks * 100)}");
            Console.WriteLine($"Number of unique antinodes (Pairwise method): {pairwiseAntinodes.Count}");

            // Part 2: Compute using Line-drawing method
            startTime = DateTime.UtcNow;
            var lineAntinodes = ComputeAntinodesLines(grid);
            var part2Time = DateTime.UtcNow - startTime;
            Console.WriteLine($"Part 002 finished in {FormatTime(part2Time.Ticks * 100)}");
            Console.WriteLine($"Number of unique antinodes (Line-drawing method): {lineAntinodes.Count}");

            // Overall timing ends here
            var overallEnd = DateTime.UtcNow;

            // Compute total computation time (sum of part1Time and part2Time)
            var computationTotalTime = part1Time + part2Time;
            Console.WriteLine($"Total computation time (sum of parts): {FormatTime(computationTotalTime.Ticks * 100)}");

            // Compute overall runtime (includes overhead)
            var overallTotalTime = overallEnd - overallStart;
            Console.WriteLine($"Overall total time (includes overhead): {FormatTime(overallTotalTime.Ticks * 100)}");
        }

    }
}
