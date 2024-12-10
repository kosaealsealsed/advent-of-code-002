using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;

class _2024_010
{
    private static readonly string INPUT_FILE = @"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-010\input.txt";

    static void Main(string[] args)
    {
        var stopwatchPart1 = Stopwatch.StartNew();
        var grid = ReadMap(INPUT_FILE);
        int totalScore = FindTrailheadScores(grid);
        stopwatchPart1.Stop();
        Console.WriteLine($"Part 1 Result: {totalScore}");
        Console.WriteLine($"Time taken for Part 1: {stopwatchPart1.Elapsed.TotalSeconds:F9} s");

        var stopwatchPart2 = Stopwatch.StartNew();
        int totalRating = CalculateTotalRating(grid);
        stopwatchPart2.Stop();
        Console.WriteLine($"Part 2 Result: {totalRating}");
        Console.WriteLine($"Time taken for Part 2: {stopwatchPart2.Elapsed.TotalSeconds:F9} s");
    }

    private static List<List<int>> ReadMap(string filename)
    {
        var grid = new List<List<int>>();
        var lines = File.ReadAllLines(filename);
        foreach (var line in lines)
        {
            if (!string.IsNullOrWhiteSpace(line))
            {
                var row = new List<int>();
                foreach (var ch in line.Trim())
                {
                    row.Add(ch - '0');
                }
                grid.Add(row);
            }
        }
        return grid;
    }

    private static List<(int, int)> Neighbors(int r, int c, int rows, int cols)
    {
        var directions = new List<(int, int)> { (-1, 0), (1, 0), (0, -1), (0, 1) };
        var result = new List<(int, int)>();
        foreach (var (dr, dc) in directions)
        {
            int nr = r + dr, nc = c + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols)
            {
                result.Add((nr, nc));
            }
        }
        return result;
    }

    private static int FindTrailheadScores(List<List<int>> grid)
    {
        int rows = grid.Count;
        int cols = rows > 0 ? grid[0].Count : 0;

        var trailheads = new List<(int, int)>();
        for (int r = 0; r < rows; r++)
        {
            for (int c = 0; c < cols; c++)
            {
                if (grid[r][c] == 0)
                {
                    trailheads.Add((r, c));
                }
            }
        }

        int totalScore = 0;

        foreach (var (startR, startC) in trailheads)
        {
            var visited = new HashSet<(int, int)>();
            var queue = new Queue<(int, int)>();
            queue.Enqueue((startR, startC));
            visited.Add((startR, startC));

            var reachableNines = new HashSet<(int, int)>();

            while (queue.Count > 0)
            {
                var (r, c) = queue.Dequeue();
                int currentHeight = grid[r][c];

                if (currentHeight == 9)
                {
                    reachableNines.Add((r, c));
                }
                else
                {
                    int nextHeight = currentHeight + 1;
                    foreach (var (nr, nc) in Neighbors(r, c, rows, cols))
                    {
                        if (!visited.Contains((nr, nc)) && grid[nr][nc] == nextHeight)
                        {
                            visited.Add((nr, nc));
                            queue.Enqueue((nr, nc));
                        }
                    }
                }
            }

            totalScore += reachableNines.Count;
        }

        return totalScore;
    }

    private static int CalculateTotalRating(List<List<int>> grid)
    {
        int rows = grid.Count;
        int cols = rows > 0 ? grid[0].Count : 0;

        var trailheads = new List<(int, int)>();
        for (int r = 0; r < rows; r++)
        {
            for (int c = 0; c < cols; c++)
            {
                if (grid[r][c] == 0)
                {
                    trailheads.Add((r, c));
                }
            }
        }

        var dp = new int[rows, cols];
        for (int r = 0; r < rows; r++)
        {
            for (int c = 0; c < cols; c++)
            {
                dp[r, c] = -1;
            }
        }

        int totalRating = 0;
        foreach (var (tr, tc) in trailheads)
        {
            totalRating += CountPaths(tr, tc, grid, dp, rows, cols);
        }

        return totalRating;
    }

    private static int CountPaths(int r, int c, List<List<int>> grid, int[,] dp, int rows, int cols)
    {
        if (dp[r, c] != -1)
        {
            return dp[r, c];
        }

        int currentHeight = grid[r][c];

        if (currentHeight == 9)
        {
            dp[r, c] = 1;
            return 1;
        }

        int totalPaths = 0;
        int nextHeight = currentHeight + 1;
        foreach (var (nr, nc) in Neighbors(r, c, rows, cols))
        {
            if (grid[nr][nc] == nextHeight)
            {
                totalPaths += CountPaths(nr, nc, grid, dp, rows, cols);
            }
        }

        dp[r, c] = totalPaths;
        return totalPaths;
    }
}
