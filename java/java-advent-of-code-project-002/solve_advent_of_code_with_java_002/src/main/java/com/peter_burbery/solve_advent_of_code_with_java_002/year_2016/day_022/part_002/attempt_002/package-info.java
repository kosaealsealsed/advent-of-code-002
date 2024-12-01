/**
 * This is based on the Python
 * import re
from collections import deque
import time
import heapq

# Read the input data
with open('input.txt') as f:
    lines = f.readlines()

# Parse the input data
nodes = {}
max_x = max_y = 0
for line in lines[1:]:
    match = re.match(r'/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T', line)
    if match:
        x, y, size, used, avail = map(int, match.groups())
        nodes[(x, y)] = {'size': size, 'used': used, 'avail': avail}
        max_x = max(max_x, x)
        max_y = max(max_y, y)

# Build the grid
grid = [['.' for _ in range(max_x + 1)] for _ in range(max_y + 1)]
empty_node = None
for (x, y), node in nodes.items():
    if node['used'] == 0:
        grid[y][x] = '_'
        empty_node = (x, y)
    elif node['used'] > 100:  # Adjust threshold based on data
        grid[y][x] = '#'
    if y == 0 and x == max_x:
        goal_data = (x, y)

# Function to calculate heuristic
def heuristic(goal_pos):
    # Estimate the minimal number of steps required to bring the goal data to (0,0)
    # Each move of the goal data requires moving the empty space next to it and swapping
    x, y = goal_pos
    return abs(x - 0) + abs(y - 0)

# Function to find the minimal steps using A* search
def find_min_steps(grid, empty_node, goal_data):
    import time
    from copy import deepcopy

    start_time = time.time()
    last_report_time = start_time

    visited = set()
    # Priority queue for A* search
    heap = []
    steps_checked = 0  # Counter for progress reporting
    report_interval = 1000  # Adjust as needed

    # Initial state
    state = (heuristic(goal_data), 0, grid, empty_node, goal_data)
    heapq.heappush(heap, state)

    while heap:
        f_score, steps, grid_state, empty_pos, goal_pos = heapq.heappop(heap)
        steps_checked += 1

        # Progress reporting
        if steps_checked % report_interval == 0:
            current_time = time.time()
            time_since_last_report = current_time - last_report_time
            total_elapsed_time = current_time - start_time
            print(f"Searched {steps_checked} states. "
                  f"Time since last report: {time_since_last_report:.2f}s, "
                  f"Total elapsed time: {total_elapsed_time:.2f}s.")
            last_report_time = current_time

        if goal_pos == (0, 0):
            return steps

        key = (tuple(map(tuple, grid_state)), empty_pos, goal_pos)
        if key in visited:
            continue
        visited.add(key)

        x_empty, y_empty = empty_pos
        for dx, dy in [(-1,0),(1,0),(0,-1),(0,1)]:
            nx_empty, ny_empty = x_empty + dx, y_empty + dy
            if 0 <= nx_empty <= max_x and 0 <= ny_empty <= max_y and grid_state[ny_empty][nx_empty] != '#':
                # Create a new grid state
                new_grid = [row[:] for row in grid_state]
                # Swap empty node with its neighbor
                new_grid[y_empty][x_empty], new_grid[ny_empty][nx_empty] = new_grid[ny_empty][nx_empty], new_grid[y_empty][x_empty]
                new_empty_pos = (nx_empty, ny_empty)
                new_goal_pos = goal_pos
                # If we moved the empty space into the goal data's position, we effectively moved the goal data
                if (nx_empty, ny_empty) == goal_pos:
                    new_goal_pos = (x_empty, y_empty)
                # Calculate new heuristic
                h = heuristic(new_goal_pos)
                # Total cost = steps taken + heuristic
                f = steps + 1 + h
                # Add new state to the priority queue
                heapq.heappush(heap, (f, steps + 1, new_grid, new_empty_pos, new_goal_pos))

    return -1  # If the goal is unreachable

# Find the minimal steps
min_steps = find_min_steps(grid, empty_node, goal_data)

# Output the result
print(f"Minimal steps required: {min_steps}")

 */
/**
 * Execution time: 10.485843700 s
Minimal steps required: 213

Execution time: 12.022725300 s
Minimal steps required: 213

 */
package com.peter_burbery.solve_advent_of_code_with_java_002.year_2016.day_022.part_002.attempt_002;