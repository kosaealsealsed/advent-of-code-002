using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace _2024_016
{
    internal class Program
    {
        private static readonly int INF = int.MaxValue;

        /// <summary>
        /// Parses the maze input from a file.
        /// </summary>
        /// <param name="filePath">The path to the input file.</param>
        /// <returns>A list of strings representing the maze.</returns>
        private static List<string> ParseInput(string filePath)
        {
            return File.ReadAllLines(filePath).Select(line => line.Trim()).ToList();
        }

        /// <summary>
        /// Solves Part 1: Finds the lowest possible score to navigate the maze.
        /// </summary>
        /// <param name="mazeLines">The maze represented as a list of strings.</param>
        /// <returns>The lowest possible score.</returns>
        private static int SolveMaze(List<string> mazeLines)
        {
            int[][] directions = {
                new int[] { 0, 1 },   // East
                new int[] { 1, 0 },   // South
                new int[] { 0, -1 },  // West
                new int[] { -1, 0 }   // North
            };

            int rows = mazeLines.Count;
            int cols = mazeLines[0].Length;
            (int, int)? start = null, end = null;

            // Locate start and end positions
            for (int r = 0; r < rows; r++)
            {
                for (int c = 0; c < cols; c++)
                {
                    if (mazeLines[r][c] == 'S')
                        start = (r, c);
                    else if (mazeLines[r][c] == 'E')
                        end = (r, c);
                }
            }

            if (start == null || end == null)
                throw new Exception("Could not find 'S' or 'E' in the maze.");

            int[,,] dist = new int[rows, cols, 4];
            for (int r = 0; r < rows; r++)
                for (int c = 0; c < cols; c++)
                    for (int d = 0; d < 4; d++)
                        dist[r, c, d] = INF;

            var pq = new PriorityQueue<(int cost, int row, int col, int dir)>();
            dist[start.Value.Item1, start.Value.Item2, 0] = 0;
            pq.Enqueue((0, start.Value.Item1, start.Value.Item2, 0));

            var visited = new HashSet<string>();

            while (pq.Count > 0)
            {
                var (cost, r, c, d) = pq.Dequeue();

                if ((r, c) == end.Value)
                    return cost;

                string stateKey = $"{r},{c},{d}";
                if (visited.Contains(stateKey))
                    continue;
                visited.Add(stateKey);

                // Move forward
                int nr = r + directions[d][0];
                int nc = c + directions[d][1];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] != '#')
                {
                    int newCost = cost + 1;
                    if (newCost < dist[nr, nc, d])
                    {
                        dist[nr, nc, d] = newCost;
                        pq.Enqueue((newCost, nr, nc, d));
                    }
                }

                // Turn left
                int leftDir = (d + 3) % 4; // equivalent to (d - 1 + 4) % 4
                int leftCost = cost + 1000;
                if (leftCost < dist[r, c, leftDir])
                {
                    dist[r, c, leftDir] = leftCost;
                    pq.Enqueue((leftCost, r, c, leftDir));
                }

                // Turn right
                int rightDir = (d + 1) % 4;
                int rightCost = cost + 1000;
                if (rightCost < dist[r, c, rightDir])
                {
                    dist[r, c, rightDir] = rightCost;
                    pq.Enqueue((rightCost, r, c, rightDir));
                }
            }

            return -1; // No path found
        }

        /// <summary>
        /// Solves Part 2: Finds the number of tiles on the best path(s).
        /// </summary>
        /// <param name="mazeLines">The maze represented as a list of strings.</param>
        /// <returns>The number of tiles on the best path(s).</returns>
        private static int SolvePart2(List<string> mazeLines)
        {
            int[][] directions = {
                new int[] { 0, 1 },   // East
                new int[] { 1, 0 },   // South
                new int[] { 0, -1 },  // West
                new int[] { -1, 0 }   // North
            };

            int rows = mazeLines.Count;
            int cols = mazeLines[0].Length;
            (int, int)? start = null, end = null;

            // Locate start and end positions
            for (int r = 0; r < rows; r++)
            {
                for (int c = 0; c < cols; c++)
                {
                    if (mazeLines[r][c] == 'S')
                        start = (r, c);
                    else if (mazeLines[r][c] == 'E')
                        end = (r, c);
                }
            }

            if (start == null || end == null)
                throw new Exception("Could not find 'S' or 'E' in the maze.");

            int[,,] dist = new int[rows, cols, 4];
            for (int r = 0; r < rows; r++)
                for (int c = 0; c < cols; c++)
                    for (int d = 0; d < 4; d++)
                        dist[r, c, d] = INF;

            var pq = new PriorityQueue<(int cost, int row, int col, int dir)>();
            dist[start.Value.Item1, start.Value.Item2, 0] = 0;
            pq.Enqueue((0, start.Value.Item1, start.Value.Item2, 0));

            var visited = new HashSet<string>();

            while (pq.Count > 0)
            {
                var (cost, r, c, d) = pq.Dequeue();

                string stateKey = $"{r},{c},{d}";
                if (visited.Contains(stateKey))
                    continue;
                visited.Add(stateKey);

                // Move forward
                int nr = r + directions[d][0];
                int nc = c + directions[d][1];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && mazeLines[nr][nc] != '#')
                {
                    int newCost = cost + 1;
                    if (newCost < dist[nr, nc, d])
                    {
                        dist[nr, nc, d] = newCost;
                        pq.Enqueue((newCost, nr, nc, d));
                    }
                }

                // Turn left
                int leftDir = (d + 3) % 4;
                int leftCost = cost + 1000;
                if (leftCost < dist[r, c, leftDir])
                {
                    dist[r, c, leftDir] = leftCost;
                    pq.Enqueue((leftCost, r, c, leftDir));
                }

                // Turn right
                int rightDir = (d + 1) % 4;
                int rightCost = cost + 1000;
                if (rightCost < dist[r, c, rightDir])
                {
                    dist[r, c, rightDir] = rightCost;
                    pq.Enqueue((rightCost, r, c, rightDir));
                }
            }

            int minCostEnd = Enumerable.Range(0, 4).Min(d => dist[end.Value.Item1, end.Value.Item2, d]);
            if (minCostEnd == INF)
                return 0;

            bool[,] onBestPath = new bool[rows, cols];
            var queue = new Queue<(int row, int col, int dir)>();
            for (int d = 0; d < 4; d++)
            {
                if (dist[end.Value.Item1, end.Value.Item2, d] == minCostEnd)
                    queue.Enqueue((end.Value.Item1, end.Value.Item2, d));
            }

            var visitedReverse = new HashSet<string>();

            while (queue.Count > 0)
            {
                var (r, c, d) = queue.Dequeue();
                onBestPath[r, c] = true;

                int costHere = dist[r, c, d];

                // Reverse move forward
                int nr = r - directions[d][0];
                int nc = c - directions[d][1];
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols)
                {
                    if (mazeLines[nr][nc] != '#' && dist[nr, nc, d] == costHere - 1)
                    {
                        string reverseKey = $"{nr},{nc},{d}";
                        if (!visitedReverse.Contains(reverseKey))
                        {
                            visitedReverse.Add(reverseKey);
                            queue.Enqueue((nr, nc, d));
                        }
                    }
                }

                // Reverse turns
                foreach (var prevDir in new int[] { (d + 3) % 4, (d + 1) % 4 })
                {
                    if (dist[r, c, prevDir] == costHere - 1000)
                    {
                        string reverseKey = $"{r},{c},{prevDir}";
                        if (!visitedReverse.Contains(reverseKey))
                        {
                            visitedReverse.Add(reverseKey);
                            queue.Enqueue((r, c, prevDir));
                        }
                    }
                }
            }

            return onBestPath.Cast<bool>().Count(isBest => isBest);
        }

        static void Main(string[] args)
        {
            string filePath = @"\\vmware-host\Shared Folders\C\advent-of-code-002\input-files\2024\2024-016\input.txt";

            try
            {
                var mazeLines = ParseInput(filePath);

                int part1Result = SolveMaze(mazeLines);
                Console.WriteLine($"Lowest possible score (Part 1): {part1Result}");

                int part2Result = SolvePart2(mazeLines);
                Console.WriteLine($"Number of tiles on at least one best path (Part 2): {part2Result}");
            }
            catch (Exception e)
            {
                Console.WriteLine($"Error: {e.Message}");
            }
        }
    }

    /// <summary>
    /// A simple priority queue implementation for tuples.
    /// </summary>
    public class PriorityQueue<T>
    {
        private readonly List<T> _heap = new();
        private readonly Comparison<T> _comparator;

        public PriorityQueue(Comparison<T>? comparator = null)
        {
            _comparator = comparator ?? Comparer<T>.Default.Compare;
        }

        public int Count => _heap.Count;

        public void Enqueue(T item)
        {
            _heap.Add(item);
            SiftUp(_heap.Count - 1);
        }

        public T Dequeue()
        {
            var top = _heap[0];
            var last = _heap[^1];
            _heap.RemoveAt(_heap.Count - 1);
            if (_heap.Count > 0)
            {
                _heap[0] = last;
                SiftDown(0);
            }
            return top;
        }

        private void SiftUp(int index)
        {
            while (index > 0)
            {
                int parent = (index - 1) / 2;
                if (_comparator(_heap[index], _heap[parent]) >= 0) break;
                (_heap[parent], _heap[index]) = (_heap[index], _heap[parent]);
                index = parent;
            }
        }

        private void SiftDown(int index)
        {
            while (true)
            {
                int left = index * 2 + 1;
                int right = left + 1;
                int smallest = index;

                if (left < _heap.Count && _comparator(_heap[left], _heap[smallest]) < 0)
                    smallest = left;
                if (right < _heap.Count && _comparator(_heap[right], _heap[smallest]) < 0)
                    smallest = right;
                if (smallest == index) break;

                (_heap[index], _heap[smallest]) = (_heap[smallest], _heap[index]);
                index = smallest;
            }
        }
    }
}
