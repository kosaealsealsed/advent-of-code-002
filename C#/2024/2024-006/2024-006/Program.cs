using System;
using System.IO;
using System.Collections.Generic;
using System.Diagnostics;

namespace _2024_006
{
    public class GuardPatrolLoopDetector
    {
        // Direction mappings
        private static readonly Dictionary<char, int> DIRECTION_MAP = new Dictionary<char, int>
        {
            { '^', 0 },
            { '>', 1 },
            { 'v', 2 },
            { '<', 3 }
        };

        private static readonly int[][] DIRECTION_OFFSETS = new int[][]
        {
            new int[] { -1, 0 }, // Up
            new int[] { 0, 1 },  // Right
            new int[] { 1, 0 },  // Down
            new int[] { 0, -1 }  // Left
        };

        public static void Main(string[] args)
        {
            string filePath = @"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-006\input.txt"; // Specify the path to your input file here
            try
            {
                // Part 1: Count distinct positions visited without obstructions
                int distinctPositions = CountDistinctPositionsVisited(filePath);
                Console.WriteLine("Number of distinct positions visited: " + distinctPositions);

                // Part 2: Detect loops with obstructions and measure execution times
                CountObstructionPositions(filePath);
            }
            catch (Exception e)
            {
                Console.Error.WriteLine("Error: " + e.Message);
            }
        }

        /// <summary>
        /// Part 1: Counts the number of distinct positions visited by the guard without any obstructions.
        /// </summary>
        /// <param name="filePath">Path to the input file.</param>
        /// <returns>Number of distinct positions visited.</returns>
        private static int CountDistinctPositionsVisited(string filePath)
        {
            // Parse the grid
            char[][] grid = ParseGrid(filePath);

            // Find the guard's starting position and direction
            var guardInfo = FindGuard(grid);
            Position guardPos = guardInfo.Item1;
            int guardDir = guardInfo.Item2;

            // Initialize visited positions set
            HashSet<Position> visitedPositions = new HashSet<Position>();
            visitedPositions.Add(guardPos);

            // Simulate the guard's movement
            while (true)
            {
                int dr = DIRECTION_OFFSETS[guardDir][0];
                int dc = DIRECTION_OFFSETS[guardDir][1];
                int newR = guardPos.Row + dr;
                int newC = guardPos.Col + dc;

                // Check boundaries
                if (newR < 0 || newR >= grid.Length || newC < 0 || newC >= grid[0].Length)
                {
                    break; // Guard exits the mapped area
                }

                if (grid[newR][newC] == '#')
                {
                    // Turn right if obstacle ahead
                    guardDir = (guardDir + 1) % 4;
                }
                else
                {
                    // Move forward
                    guardPos = new Position(newR, newC);
                    visitedPositions.Add(guardPos);
                }
            }

            // Number of distinct positions visited
            return visitedPositions.Count;
        }

        /// <summary>
        /// Part 2: Counts the number of obstruction positions that cause the guard to loop indefinitely.
        /// Also measures and prints execution times.
        /// </summary>
        /// <param name="filePath">Path to the input file.</param>
        private static void CountObstructionPositions(string filePath)
        {
            // Start total timing
            var totalStopwatch = Stopwatch.StartNew();

            // Parse the grid
            char[][] grid = ParseGrid(filePath);

            // Find the guard's starting position and direction
            var guardInfo = FindGuard(grid);
            Position guardPos = guardInfo.Item1;
            int guardDir = guardInfo.Item2;

            // Time to find obstruction positions
            var obstructionStopwatch = Stopwatch.StartNew();
            List<Position> possibleObstructions = GetPossibleObstructions(grid, guardPos);
            obstructionStopwatch.Stop();
            double obstructionTime = obstructionStopwatch.Elapsed.TotalSeconds;

            // Print the first header and line: [time_obstruction_positions] [total_obstruction_positions]
            Console.WriteLine("time, denominator");
            Console.WriteLine($"{obstructionTime:F9} {possibleObstructions.Count}");

            // Print header for batches
            Console.WriteLine("batch, batch time, cumulative time");

            // Initialize loop counter
            int loopCount = 0;
            int total = possibleObstructions.Count;

            // Initialize timing for batches
            const int batchSize = 1000;
            var batchStopwatch = Stopwatch.StartNew();
            double cumulativeTime = obstructionTime; // cumulative_time includes obstruction_time

            for (int idx = 0; idx < possibleObstructions.Count; idx++)
            {
                Position obstruction = possibleObstructions[idx];
                grid[obstruction.Row][obstruction.Col] = '#'; // Place obstruction

                if (SimulateMovement(grid, guardPos, guardDir))
                {
                    loopCount++; // Found a position that causes a loop
                }

                grid[obstruction.Row][obstruction.Col] = '.'; // Remove obstruction

                // Check if batch size is reached or it's the last position
                if ((idx + 1) % batchSize == 0 || (idx + 1) == total)
                {
                    batchStopwatch.Stop();
                    double batchTime = batchStopwatch.Elapsed.TotalSeconds;
                    cumulativeTime += batchTime;
                    Console.WriteLine($"{idx + 1} {batchTime:F9} {cumulativeTime:F9}");
                    batchStopwatch.Restart(); // Reset batch start time
                }
            }

            // End total timing
            totalStopwatch.Stop();
            double totalTime = totalStopwatch.Elapsed.TotalSeconds; // Total time from start to end

            // Print final answer header and line: [answer] [answer_time]
            Console.WriteLine("answer, answer time");
            Console.WriteLine($"{loopCount} {totalTime:F9}");
        }

        /// <summary>
        /// Parses the grid from the given file.
        /// </summary>
        /// <param name="filePath">Path to the input file.</param>
        /// <returns>2D character array representing the grid.</returns>
        private static char[][] ParseGrid(string filePath)
        {
            var gridList = new List<char[]>();
            using (var br = new StreamReader(filePath))
            {
                string line;
                while ((line = br.ReadLine()) != null)
                {
                    gridList.Add(line.Trim().ToCharArray());
                }
            }
            if (gridList.Count == 0)
            {
                throw new ArgumentException("The grid is empty.");
            }
            // Ensure all rows have the same length
            int cols = gridList[0].Length;
            foreach (char[] row in gridList)
            {
                if (row.Length != cols)
                {
                    throw new ArgumentException("Inconsistent row lengths in the grid.");
                }
            }
            return gridList.ToArray();
        }

        /// <summary>
        /// Finds the guard's starting position and direction.
        /// </summary>
        /// <param name="grid">2D character array representing the grid.</param>
        /// <returns>A tuple containing the starting Position and direction.</returns>
        private static (Position, int) FindGuard(char[][] grid)
        {
            for (int r = 0; r < grid.Length; r++)
            {
                for (int c = 0; c < grid[0].Length; c++)
                {
                    char cell = grid[r][c];
                    if (DIRECTION_MAP.ContainsKey(cell))
                    {
                        Position guardPos = new Position(r, c);
                        int guardDir = DIRECTION_MAP[cell];
                        grid[r][c] = '.'; // Clear the starting position
                        return (guardPos, guardDir);
                    }
                }
            }
            throw new ArgumentException("Guard not found in the grid.");
        }

        /// <summary>
        /// Retrieves all possible obstruction positions excluding the guard's starting position and already obstructed cells.
        /// </summary>
        /// <param name="grid">2D character array representing the grid.</param>
        /// <param name="guardPos">The starting position of the guard.</param>
        /// <returns>List of possible obstruction Positions.</returns>
        private static List<Position> GetPossibleObstructions(char[][] grid, Position guardPos)
        {
            var possible = new List<Position>();
            for (int r = 0; r < grid.Length; r++)
            {
                for (int c = 0; c < grid[0].Length; c++)
                {
                    if ((r != guardPos.Row || c != guardPos.Col) && grid[r][c] == '.')
                    {
                        possible.Add(new Position(r, c));
                    }
                }
            }
            return possible;
        }

        /// <summary>
        /// Simulates the guard's movement on the grid.
        /// </summary>
        /// <param name="grid">2D character array representing the grid.</param>
        /// <param name="startPos">Starting position of the guard.</param>
        /// <param name="startDir">Starting direction of the guard.</param>
        /// <returns>True if a loop is detected, False if the guard exits the grid.</returns>
        private static bool SimulateMovement(char[][] grid, Position startPos, int startDir)
        {
            var visitedStates = new HashSet<State>();
            int r = startPos.Row;
            int c = startPos.Col;
            int direction = startDir;

            while (true)
            {
                var currentState = new State(r, c, direction);
                if (visitedStates.Contains(currentState))
                {
                    return true; // Loop detected
                }
                visitedStates.Add(currentState);

                int dr = DIRECTION_OFFSETS[direction][0];
                int dc = DIRECTION_OFFSETS[direction][1];
                int newR = r + dr;
                int newC = c + dc;

                // Check boundaries
                if (newR < 0 || newR >= grid.Length || newC < 0 || newC >= grid[0].Length)
                {
                    return false; // Guard exits the grid
                }

                if (grid[newR][newC] == '#')
                {
                    // Turn right if obstacle ahead
                    direction = (direction + 1) % 4;
                }
                else
                {
                    // Move forward
                    r = newR;
                    c = newC;
                }
            }
        }

        /// <summary>
        /// Record to represent a position in the grid.
        /// </summary>
        private record Position(int Row, int Col);

        /// <summary>
        /// Record to represent a state (position and direction) of the guard.
        /// </summary>
        private record State(int Row, int Col, int Direction);
    }
}
